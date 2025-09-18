/**
 * @file Gosu is a statically typed general-purpose programming language that runs on the Java Virtual Machine.
 * @author Marek Kubica <marek@tarides.com>
 * @license UNLICENSED
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check


// Based on the grammer at: https://github.com/gosu-lang/gosu-lang/blob/78c5f6c839597a81ac5ec75a46259cbb6ad40545/gosu-core/src/main/java/gw/internal/gosu/parser/ebnf/Gosu.ebnf

// These patterns are used in the "Ident" rule. In order to make that rule
// produce a token these can't be rules themselves as tokens must composed of
// only terminal rules.
const Digit = /[0-9]/;
const Letter = /[A-Za-z_$]/;

// The rules, presented in the same order as the grammar definition in Gosu's source code.
module.exports = grammar({
  name: "gosu",

  extras: $ => [
    /\s/,
    $.COMMENT,
    $.LINE_COMMENT,
  ],

  rules: {
    start: $ => seq(
      optional(seq("package", $.namespaceStatement)),
      repeat($.usesStatement),
      optional($.modifiers),
      choice($.gClass,
             // $.gInterfaceOrStructure, $.gEnum,
             // $.gEnhancement

             // NOTE: Allow we allow functionDefn, statement, etc. for parsing
             // incomplete files, adding some support for Gosu programs and
             // parsing patterns in Semgrep rules, even if this is not strictly
             // allowed in Gosu's EBNF grammar. See also tree-sitter-java, for
             // instance.
             $.functionDefn,
             $.statement,
            ),
    ),
    namespaceStatement: $ => seq($.id, repeat(seq(".", $.id)), repeat(";")),
    usesStatement: $ => seq("uses", seq($.id, repeat(seq(".", $.id)), optional(seq(".", "*"))), repeat(";")),
    modifiers: $ =>
      repeat1(choice(
        // $.annotation,
        "private", "internal", "protected", "public",
        "static", "abstract", "override", "final", "transient")),

    gClass: $ => seq(
      "class",
      field("name", $.id),
      // PossiblyEmpty.typeVariableDefs($),
      // optional(seq("extends", $.classOrInterfaceType)),
      // optional(seq("implements", $.classOrInterfaceType, repeat(seq(",", $.classOrInterfaceType)))),
      field("body", $.classBody),
    ),
    classBody: $ => seq("{", optional($.classMembers), "}"),
    classMembers: $ => repeat1($.declaration),
    declaration: $ => choice(
      $.fieldDefn,
      $.functionDefn,
      // TODO
    ),
    fieldDefn: $ => seq(
      optional($.modifiers),
      "var",
      $.id,
      optional(seq(":",
        field("type", $.type))),
      optional(seq("as",
        optional("readonly"), field("as", $.id))),
      optional(seq("=",
        field("value", $._expression))),
      optional(";")
    ),
    functionDefn: $ => seq(
      "function", $.id, "(", optional($.parameterDeclarationList), ")",
      optional(seq("{", repeat($.statement), "}"))
    ),
    parameterDeclarationList: $ => seq($.parameterDeclaration, repeat(seq(",", $.parameterDeclaration))),
    parameterDeclaration: $ => seq(
      $.id,
      optional(choice(
        seq(":", field("type", $.type), optional(seq("=", field("value", $._expression)))),
        // TODO: $.blockTypeLiteral,
        seq("=", field("value", $._expression)),
      )),
    ),
    _statement: $ => choice(
      $.localVarStatement,
      $.assignmentOrMethodCall,
    ),
    statement: $ => seq($._statement, optional(";")),
    localVarStatement: $ => seq(
      "var",
      $.id,
      optional(seq(":",
        field("type", $.type))),
      optional(seq("=",
        field("value", $._expression))),
    ),
    arguments: $ => seq("(", optional(seq($._expression, repeat(seq(",", $._expression)))), ")"),
    indirectMemberAccess1: $ => choice(
      seq(".", $.id),
      $.arguments,
    ),
    assignmentOrMethodCall: $ => seq(
      choice(
        $.id,
        $.newExpr,
      ),
      repeat($.indirectMemberAccess1),
    ),
    type: $ =>
      choice($._type_identifier)
      // prec.right(choice(seq($.classOrInterfaceType, repeat(seq("[", "]"))), seq("block", $.blockLiteral)));
    ,
    _type_identifier: $ => alias($.id, $.type_identifier),
    _expression: $ => prec.right(seq(
      choice(
        $.StringLiteral,
        $.id,
        $.additiveExpr,
        $.newExpr,
      ),
      repeat($.indirectMemberAccess1),
    )),
    // FIXME: express in terms of multiplicativeExpr as in EBNF?
    additiveExpr: $ => prec.left(seq($._expression, seq($.additiveOp, $._expression))),
    newExpr: $ => seq("new", $.id, $.arguments),
    id: $ => token(seq(Letter, repeat(choice(Digit, Letter)))) ,
    StringLiteral: $ => choice(
      seq("'", /[^'\\]+/, "'"),
      seq('"', /[^"\\]+/, '"')
    ),
    Digit: _ => Digit,
    additiveOp: _ => choice("+", "-", "?+", "?-"),
    // HexDigit: $ => choice($.Digit, /[A-F]/, /[a-f]/),
    // ZeroToSeven: _ => /[0-7]/,
    // EscapeSequence: $ => choice(seq("\\", choice("v", "a", "b", "t", "n", "f", "r", '"', "'", "\\", "$", "<")), $.UnicodeEscape, $.OctalEscape),
    // OctalEscape: $ => choice(seq("\\", /0-3/, $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven)),
    // UnicodeEscape: $ => seq("\\u", $.HexDigit, $.HexDigit, $.HexDigit, $.HexDigit),
    COMMENT: _ => /\/\*([^\*]|(\*[^\/]))*\*\//,
    LINE_COMMENT: _ => /\/\/[^\n\r]*/,
  }
});

// `optionalSeq(a, b, c, ...)` is
// `choice(
//   seq(a, optional(b), optional(c)),
//   seq(optional(a), b, optional(c)),
//   seq(optional(a), optional(b), c)
//  )`
//  This gives us a tree-sitter compatible representation of rules like
//  `header = ["package" namespaceStatement] [usesStatementList] .`
//  where every subrule in the sequence is optional.
function optionalSeq(...rules) {
  if (rules.length === 0) {
    throw new Error("optionalSeq requires at least one rule.");
  }
  let optional_rules = rules.map(
    (_, non_optional_rule_idx) => {
      let all_but_one_rule_optional = rules.map(
        (rule, rule_idx) =>
          non_optional_rule_idx === rule_idx
            ? rule
            : optional(rule)
      )
      return seq(...all_but_one_rule_optional)
    }
  )
  return choice(...optional_rules)
}

// TODO: Maybe use these rules?
// header: $ => optionalSeq(
//   seq("package", $.namespaceStatement),
//   $.usesStatementList
// ),
// annotation: $ => seq("@", $.idAll, repeat(seq(".", $.idAll)), optional($.annotationArguments)),
// gInterfaceOrStructure: $ => seq(
//   choice("interface", "structure"),
//   $.id,
//   PossiblyEmpty.typeVariableDefs($),
//   optional(seq("extends", $.classOrInterfaceType, repeat(seq(",", $.classOrInterfaceType)))),
//   // $.interfaceBody,
// ),
// gEnum: $ => seq(
//   "enum",
//   $.id,
//   PossiblyEmpty.typeVariableDefs($),
//   optional(seq("implements", $.classOrInterfaceType, repeat(seq(",", $.classOrInterfaceType)))),
//   $.enumBody,
// ),
// // gEnhancement: $ => seq(
// //   "enhancement",
// //   $.id,
// //   PossiblyEmpty.typeVariableDefs($),
// //   ":",
// //   $.classOrInterfaceType,
// //   repeat(seq("[", "]")),
// //   $.enhancementBody,
// // ),
// // enhancementBody: $ => seq("{", PossiblyEmpty.enhancementMembers($), "}"),
// // interfaceBody: $ => seq("{", PossiblyEmpty.interfaceMembers($), "}"),
// enumBody: $ => seq("{", optional($.enumConstants), PossiblyEmpty.classMembers($), "}"),
// enumConstants: $ => seq($.enumConstant, repeat(seq(",", $.enumConstant)), optional(","), optional(";")),
// enumConstant: $ => seq($.id, PossiblyEmpty.optionalArguments($)),
// // "interfaceMembers" is possibly empty
// // "classMembers" is possibly empty
//   seq($.functionDefn, optional($.functionBody)),
//   seq($.constructorDefn, $.functionBody),
//   seq($.propertyDefn, optional($.functionBody)),
//   $.delegateDefn,
//   $.gClass,
//   $.gInterfaceOrStructure,
//   $.gEnum,
// // "enhancementMembers" is possibly empty
// delegateDefn: $ => seq("delegate", $.id, $.delegateStatement),
// delegateStatement: $ => seq(
//   optional(seq(":", $.typeLiteral)),
//   "represents",
//   $.typeLiteral,
//   repeat(seq(",", $.typeLiteral)),
//   optional(seq("=", $.expression)),
// ),
// // "optionalType" is possibly empty
// propertyDefn: $ => seq("property", choice("get", "set"), $.id, $.parameters, optional(seq(":", $.typeLiteral))),
// dotPathWord: $ => prec.right(seq($.idAll, repeat(seq(".", $.idAll)))),
// namespaceStatement: $ => seq($.dotPathWord, repeat(";")),
// usesStatementList: $ => seq("uses", $.usesStatement, repeat(seq("uses", $.usesStatement))),
// usesStatement: $ => seq($.dotPathWord, optional(seq(".", "*")), repeat(";")),
// // "typeVariableDefs" is possibly empty
// typeVariableDefinition: $ => seq($.id, optional(seq("extends", $.typeLiteralList))),
// functionBody: $ => $.statementBlock,
// parameters: $ => seq("(", optional($.parameterDeclarationList), ")"),
// functionDefn: $ => seq("function", $.id, PossiblyEmpty.typeVariableDefs($), $.parameters, optional(seq(":", $.typeLiteral))),
// constructorDefn: $ => seq("construct", $.parameters, optional(seq(":", $.typeLiteral))),
// // "modifiers" is possibly empty
// statement: $ => {
//   // Gosu's grammar explicitly that statements may end with semicolons, but
//   // also that a semicolon on its own is a statement. This is ambiguous
//   // since it's unclear whether the following should be considered one or
//   // two statements: `foo;`. Statements are parsed right-associatively so
//   // that statements ending with semicolons are treated as single
//   // statements rather than a pair of statements.
//   return choice(prec.right(seq(choice(
//     $.ifStatement,
//     $.tryCatchFinallyStatement,
//     $.throwStatement,
//     "continue",
//     "break",
//     $.returnStatement,
//     $.forEachStatement,
//     $.whileStatement,
//     $.doWhileStatement,
//     $.switchStatement,
//     $.usingStatement,
//     $.assertStatement,
//     seq("final", $.localVarStatement),
//     $.localVarStatement,
//     $.evalExpr,
//     $.assignmentOrMethodCall,
//     $.statementBlock,
//   ), optional(";"))), ";");
// },
// ifStatement: $ => {
//   // Right-associative so that the semicolon after the statement is considered part of the if-statement
//   return prec.right(seq("if", "(", $.expression, ")", $.statement, optional(";"), optional(seq("else", $.statement))));
// },
// tryCatchFinallyStatement: $ => seq("try", $.statementBlock, choice(
//   seq($.catchClause, repeat($.catchClause), optional(seq("finally", $.statementBlock))),
//   seq("finally", $.statementBlock),
// )),
// catchClause: $ => seq("catch", "(", optional("var"), $.id, optional(seq(":", $.typeLiteral)), ")", $.statementBlock),
// assertStatement: $ => seq("assert", $.expression, optional(seq(":", $.expression))),
// usingStatement: $ => seq(
//   "using",
//   "(",
//   choice(seq($.localVarStatement, repeat(seq(",", $.localVarStatement))), $.expression),
//   ")",
//   $.statementBlock,
//   optional(seq("finally", $.statementBlock)),
// ),
// returnStatement: $ => prec.right(seq("return", optional($.expression))),
// whileStatement: $ => seq("while", "(", $.expression, ")", $.statement),
// doWhileStatement: $ => seq("do", $.statement, "while", "(", $.expression, ")"),
// switchStatement: $ => seq("switch", "(", $.expression, ")", "{", repeat($.switchBlockStatementGroup), "}"),
// switchBlockStatementGroup: $ => seq(choice(seq("case", $.expression, ":"), seq("default", ":")), $.statement),
// throwStatement: $ => seq("throw", $.expression),
// localVarStatement: $ => {
//   // XXX check that it's correct for this to be right-associative
//   return prec.right(seq("var", $.id, PossiblyEmpty.optionalType($), optional(seq("=", $.expression))));
// },
// forEachStatement: $ => seq(
//   choice("foreach", "for"),
//   "(",
//   choice(seq($.expression, optional($.indexVar)), seq(optional("var"), $.id, "in", $.expression, optional($.indexRest))),
//   ")",
//   $.statement,
// ),
// indexRest: $ => choice(
//   seq($.indexVar, $.iteratorVar),
//   seq($.iteratorVar, $.indexVar),
//   $.indexVar,
//   $.iteratorVar,
// ),
// indexVar: $ => seq("index", $.id),
// iteratorVar: $ => seq("iterator", $.id),
// thisSuperExpr: _ => choice("this", "super"),
// assignmentOrMethodCall: $ => {
//   // Without specifying an associativity for this rule the grammar
//   // ambiguous. An example of the ambiguity is whether the following should
//   // be interpreted as one or two statements: `foo(bar)`. Parenthesized
//   // expressions (`parenthExpr`) are valid statements according to the
//   // grammar, as are identifiers, and statement separators are entirely
//   // optional. The choice of right associativity here treats the example
//   // text as a single statement since otherwise function calls would not be
//   // parsed as such.
//   return prec.right(seq(
//     choice($.newExpr, $.thisSuperExpr, $.typeLiteralExpr, $.parenthExpr, $.StringLiteral),
//     PossiblyEmpty.indirectMemberAccess($),
//     optional(choice(
//       $.incrementOp,
//       seq($.assignmentOp, $.expression),
//     )),
//   ));
// },
// statementBlock: $ => $.statementBlockBody,
// statementBlockBody: $ => seq("{", repeat($.statement), "}"),
// blockTypeLiteral: $ => $.blockLiteral,
// blockLiteral: $ => {
//   // XXX check that it's correct for this to be right-associative
//   return prec.right(seq("(", optional(seq($.blockLiteralArg, repeat(seq(",", $.blockLiteralArg)))), ")", optional(seq(":", $.typeLiteral))));
// },
// blockLiteralArg: $ => choice(
//   seq($.id, optional(choice(seq("=", $.expression), $.blockTypeLiteral))),
//   seq(optional(seq($.id, ":")), $.typeLiteral, optional(seq("=", $.expression))),
// ),
// typeLiteral: $ => {
//   // XXX check that the "&" operator is indeed left-associative
//   return prec.left(seq($.type, optional(seq("&", $.type))));
// },
// typeLiteralType: $ => $.typeLiteral,
// typeLiteralExpr: $ => $.typeLiteral,
// typeLiteralList: $ => $.typeLiteral,
// XXX check associativity
// classOrInterfaceType: $ => {
//   // XXX is it correct that this is left-associative?
//   return prec.left(seq($.idclassOrInterfaceType, PossiblyEmpty.typeArguments($), repeat(seq(".", $.id, PossiblyEmpty.typeArguments($)))));
// },
// // "typeArguments" is possibly empty
// typeArgument: $ => choice($.typeLiteralType, seq("?", optional(seq(choice("extends", "super"), $.typeLiteralType)))),
// In EBNF, defined as `conditionalExpr`
// conditionalExpr: $ => {
//   // XXX check that the associativity is correct
//   return prec.right(seq($.conditionalOrExpr, optional(choice(seq("?", $.conditionalExpr, ":", $.conditionalExpr), seq("?:", $.conditionalExpr)))));
// },
// conditionalOrExpr: $ => prec.left(seq($.conditionalAndExpr, repeat(seq($.orOp, $.conditionalAndExpr)))),
// conditionalAndExpr: $ => prec.left(seq($.bitwiseOrExpr, repeat(seq($.andOp, $.bitwiseOrExpr)))),
// bitwiseOrExpr: $ => prec.left(seq($.bitwiseXorExpr, repeat(seq("|", $.bitwiseXorExpr)))),
// bitwiseXorExpr: $ => prec.left(seq($.bitwiseAndExpr, repeat(seq("^", $.bitwiseAndExpr)))),
// bitwiseAndExpr: $ => prec.left(seq($.equalityExpr, repeat(seq("&", $.equalityExpr)))),
// equalityExpr: $ => prec.left(seq($.relationalExpr, repeat(seq($.equalityOp, $.relationalExpr)))),
// relationalExpr: $ => prec.left(seq($.intervalExpr, repeat(choice(seq($.relOp, $.intervalExpr), seq("typeis", $.typeLiteralType))))),
// intervalExpr: $ => prec.left(seq($.bitshiftExpr, optional(seq($.intervalOp, $.bitshiftExpr)))),
// bitshiftExpr: $ => prec.left(seq($.additiveExpr, repeat(seq($.bitshiftOp, $.additiveExpr)))),
// additiveExpr: $ => prec.left(seq($.multiplicativeExpr, repeat(seq($.additiveOp, $.multiplicativeExpr)))),
// multiplicativeExpr: $ => prec.left(seq($.typeAsExpr, repeat(seq($.multiplicativeOp, $.typeAsExpr)))),
// typeAsExpr: $ => prec.left(seq($.unaryExpr, repeat(seq($.typeAsOp, $.unaryExpr)))),
// unaryExpr: $ => choice(seq(choice("+", "-", "!-"), $.unaryExprNotPlusMinus), $.unaryExprNotPlusMinus),
// unaryExprNotPlusMinus: $ => {
//   /// XXX check that this precedence is correct
//   return prec(1, choice(seq($.unaryOp, $.unaryExpr), seq("\\", $.blockExpr), $.evalExpr, $.primaryExpr));
// },
// blockExpr: $ => seq(optional($.parameterDeclarationList), "->", choice($.expression, $.statementBlock)),
// parameterDeclarationList: $ => seq($.parameterDeclaration, repeat(seq(",", $.parameterDeclaration))),
// parameterDeclaration: $ => seq(
//   repeat($.annotation),
//   optional("final"),
//   $.id,
//   optional(choice(
//     seq(":", $.typeLiteral, optional(seq("=", $.expression))),
//     $.blockTypeLiteral,
//     seq("=", $.expression),
//   )),
// ),
// annotationArguments: $ => $.arguments,
//   arguments: $ => seq("(", optional(seq($.argExpression, repeat(seq(",", $.argExpression)))), ")"),
//   // "optionArguments" is possibly empty
//   argExpression: $ => choice($.namedArgExpression, $.expression),
//   namedArgExpression: $ => seq(":", $.id, "=", $.expression),
//   evalExpr: $ => seq("eval", "(", $.expression, ")"),
//   featureLiteral: $ => {
//     // XXX check that it's correct for this to be right-associative
//     return prec.right(seq("#", choice($.id, "construct"), PossiblyEmpty.typeArguments($), PossiblyEmpty.optionalArguments($)));
//   },
//   standAloneDataStructureInitialization: $ => {
//     // XXX check that the precedence is correct here
//     // This needs higher precedence than the "statementBlockBody" rule as their syntaxes overlap.
//     return prec(1, seq("{", optional($.initializerExpression), "}"));
//   },
//   primaryExpr: $ => {
//     // XXX check that it's correct for this to be right-associative
//     // XXX check that giving primaryExpr higher precedence is correct. The
//     // intuition is that if some text can be interpreted as an expression
//     // then it should be interpreted as an expression (rather than as a
//     // statement for example).
//     return prec(1, prec.right(seq(choice(
//       $.newExpr,
//       $.thisSuperExpr,
//       $.literal,
//       $.typeLiteralExpr,
//       $.parenthExpr,
//       $.standAloneDataStructureInitialization,
//     ), PossiblyEmpty.indirectMemberAccess($))));
//   },
//   parenthExpr: $ => seq("(", $.expression, ")"),
//   newExpr: $ => {
//     // XXX check associativity
//     return prec.right(seq(
//       "new",
//       optional($.classOrInterfaceType),
//       choice(
//         seq($.arguments, optional(seq("{", choice(PossiblyEmpty.initializer($), PossiblyEmpty.anonymousInnerClass($)), "}"))),
//         seq("[", choice(
//           seq("]", repeat(seq("[", "]")), $.arrayInitializer),
//           seq($.expression, "]", repeat(seq("[", $.expression, "]")), repeat(seq("[", "]"))),
//         )),
//       ),
//     ));
//   },
//   // "anonymousInnerClass" is possibly empty
//   arrayInitializer: $ => seq("{", optional(seq($.expression, repeat(seq(",", $.expression)))), "}"),
//   // "initializer" is possibly empty
//   initializerExpression: $ => choice($.mapInitializerList, $.arrayValueList),
//   arrayValueList: $ => seq($.expression, repeat(seq(",", $.expression))),
//   mapInitializerList: $ => seq($.expression, "->", $.expression, repeat(seq(",", $.expression, "->", $.expression))),
//   objectInitializer: $ => seq($.initializerAssignment, repeat(seq(",", $.initializerAssignment))),
//   initializerAssignment: $ => seq(":", $.id, "=", $.expression),
//   // "indirectMemberAccess" is possibly empty
//   literal: $ => prec(1, choice($.NumberLiteral, $.featureLiteral, $.StringLiteral, $.CharLiteral, "true", "false", "null")),
//   orOp: _ => choice("||", "or"),
//   andOp: _ => choice("&&", "and"),
//   assignmentOp: _ => choice("=", "+=", "-=", "*=", "/=", "&=", "&&=", "|=", "||=", "^=", "%=", seq("<", "<", "="), seq(">", ">", ">", "="), seq(">", ">", "=")),
//   incrementOp: _ => choice("++", "--"),
//   equalityOp: _ => choice("===", "!==", "==", "!="),
//   intervalOp: _ => choice("..", "|..", "..|", "|..|"),
//   relOp: _ => choice(seq("<", "="), seq(">", "="), "<", ">"),
//   bitshiftOp: _ => choice(seq("<", "<"), seq(">", ">", ">"), seq(">", ">")),
//   additiveOp: _ => choice("+", "-", "?+", "?-", "!+", "!-"),
//   multiplicativeOp: _ => choice("*", "/", "%", "?*", "!*", "?/", "?%"),
//   typeAsOp: _ => choice("typeas", "as"),
//   unaryOp: _ => choice("~", "!", "not", "typeof", "statictypeof"),
  // In EBNF, defined as `Ident`
  // Gosu's grammar explicitly matches keywords with this rule but keywords
  // are already naturally matched by "Ident".
  // XXX arbitrarily gave "id" precedence over "idclassOrInterfaceType". Need to check if this is correct.
//   IntegerTypeSuffix: _ => choice("l", "L", "s", "S", "bi", "BI", "b", "B"),
//   // "Letter" is defined above
//   // "NonZeroDigit" is unused in any rule
//   Exponent: $ => seq(choice("e", "E"), optional(choice("+", "-")), $.Digit, repeat($.Digit)),
//   FloatTypeSuffix: _ => choice("f", "F", "d", "D", "bd", "BD"),
//   idclassOrInterfaceType: $ => {
//     // Gosu's grammar explicitly matches keywords with this rule but keywords
//     // are already naturally matched by "Ident".
//     // XXX is it correct that this is left associative?
//     return prec.left($.Ident);
//   },
//   idAll: $ => {
//     // Gosu's grammar explicitly matches keywords with this rule but keywords
//     // are already naturally matched by "Ident".
//     return $.Ident;
//   },
//   NumberLiteral: $ => choice("NaN", "Infinity", $.HexLiteral, $.BinLiteral, $.IntOrFloatPointLiteral),
//   BinLiteral: $ => seq(choice("0b", "0B"), choice("0", "1"), repeat(choice("0", "1")), optional($.IntegerTypeSuffix)),
//   HexLiteral: $ => seq(choice("0x", "0X"), $.HexDigit, repeat($.HexDigit), optional(choice("s", "S", "l", "L"))),
//   IntOrFloatPointLiteral: $ => {
//     // XXX check associatiivty
//     return prec.right(choice(
//       seq(".", $.Digit, repeat($.Digit), optional($.FloatTypeSuffix)),
//       seq($.Digit, repeat($.Digit),
//         // In Gosu's grammar this is not explicitly optional but of the choices is the empty pattern which is equivalent but not allowed in tree-sitter.
//         optional(
//           choice(
//             seq(".", $.Digit, repeat($.Digit), optional($.Exponent), optional($.FloatTypeSuffix)),
//             seq($.Exponent, optional($.FloatTypeSuffix)),
//             $.FloatTypeSuffix,
//             $.IntegerTypeSuffix,
//           ),
//         ),
//       ),
//     ));
//   },
//   CharLiteral: $ => {
//     // In Gosu's syntax, char literals have the same syntax as
//     // single-character single-quoted string literals. Give char literals
//     // higher precedence to break the conflict.
//     return  prec(1, seq("'", choice($.EscapeSequence, /[^'\\]/), "'"));
//   },

// TODO: rm
// Pseudo-rules corresponding to rules in Gosu's grammar which match the empty
// string. Tree-sitter does not permit rules to match the empty string except
// for the start rule. These functions can be called in other rules but cannot
// be a rule on their own.
// const PossiblyEmpty = {
//   // modifiers: $ => repeat(choice($.annotation, "private", "internal", "protected", "public", "static", "abstract", "override", "final", "transient")),
//   optionalType: $ => optional(choice(seq(":", $.typeLiteral), $.blockTypeLiteral)),
//   indirectMemberAccess: $ => repeat(choice(
//     seq(choice(".", "?.", "*."), $.idAll, PossiblyEmpty.typeArguments($)),
//     $.featureLiteral,
//     seq(choice("[", "?["), $.expression, "]"),
//     $.arguments,
//   )),
//   typeVariableDefs: $ => optional(seq("<", $.typeVariableDefinition, repeat(seq(",", $.typeVariableDefinition)), ">")),
//   typeArguments: $ => optional(seq("<", $.typeArgument, repeat(seq(",", $.typeArgument)), ">")),
//   // interfaceMembers: $ => repeat(seq(
//   //   PossiblyEmpty.modifiers($),
//   //   choice(
//   //     $.functionDefn,
//   //     $.propertyDefn,
//   //     $.fieldDefn,
//   //     $.gClass,
//   //     $.gInterfaceOrStructure,
//   //     $.gEnum,
//   //   ),
//   //   optional(";"),
//   // )),
//   classMembers: $ => repeat($.declaration),
//   // enhancementMembers: $ => repeat(seq(
//   //   PossiblyEmpty.modifiers($),
//   //   choice(
//   //     seq($.functionDefn, $.functionBody),
//   //     seq($.propertyDefn, $.functionBody),
//   //   ),
//   //   optional(";"),
//   // )),
//   optionalArguments: $ => optional($.arguments),
//   initializer: $ => optional(choice($.initializerExpression, $.objectInitializer)),
//   anonymousInnerClass: $ => PossiblyEmpty.classMembers($),
// };
