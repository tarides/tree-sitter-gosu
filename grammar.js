/**
 * @file Gosu is a statically typed general-purpose programming language that runs on the Java Virtual Machine.
 * @author Marek Kubica <marek@tarides.com>
 * @license UNLICENSED
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check


// Based on the grammer at: https://github.com/gosu-lang/gosu-lang/blob/78c5f6c839597a81ac5ec75a46259cbb6ad40545/gosu-core/src/main/java/gw/internal/gosu/parser/ebnf/Gosu.ebnf

// Pseudo-rules corresponding to rules in Gosu's grammar which match the empty
// string. Tree-sitter does not permit rules to match the empty string except
// for the start rule. These functions can be called in other rules but cannot
// be a rule on their own.
const PossiblyEmpty = {
  start: $ => seq(PossiblyEmpty.header($), PossiblyEmpty.modifiers($), choice($.gClass)), // XXX incomplete
  header: $ => seq(optional(seq("package", $.namespaceStatement)), optional($.usesStatementList)),
  modifiers: $ => repeat(choice($.annotation, "private", "internal", "protected", "public", "static", "abstract", "override", "final", "transient")),
  optionalType: $ => optional(choice(seq(":", $.typeLiteral), $.blockTypeLiteral)),
  indirectMemberAccess: $ => repeat(choice($.arguments)), // XXX incomplete
  typeVariableDefs: $ => optional(seq("<", $.typeVariableDefinition, repeat(seq(",", $.typeVariableDefinition)), ">")),
  typeArguments: $ => optional(seq("<", $.typeArgument, repeat(seq(",", $.typeArgument)), ">")),
  classMembers: $ => repeat($.declaration),
  optionalArguments: $ => optional($.arguments),
};

// These patterns are used in the "Ident" rule. In order to make that rule
// produce a token these can't be rules themselves as tokens must composed of
// only terminal rules.
const Digit = /[0-9]/;
const Letter = /[A-Za-z_$]/;

module.exports = grammar({
  name: "gosu",

  extras: $ => [
    $.WS,
    $.COMMENT,
    $.LINE_COMMENT,
  ],

  rules: {
    source_file: $ => PossiblyEmpty.start($),
    // "start" is possibly empty
    // "header" is possibly empty
    annotation: $ => seq("@", $.idAll, repeat(seq(".", $.idAll)), optional($.annotationArguments)),
    gClass: $ => seq(
      "class",
      $.id,
      PossiblyEmpty.typeVariableDefs($),
      optional(seq("extends", $.classOrInterfaceType)),
      optional(seq("implements", $.classOrInterfaceType, repeat(seq(",", $.classOrInterfaceType)))),
      $.classBody,
    ), // XXX incomplete
    classBody: $ => seq("{", PossiblyEmpty.classMembers($), "}"),
    // "classMembers" is possibly empty
    declaration: $ => seq(PossiblyEmpty.modifiers($), choice(
      seq($.functionDefn, optional($.functionBody)),
      seq($.constructorDefn, $.functionBody),
      $.fieldDefn,
    ), optional(";")), // XXX incomplete
    // "optionalType" is possibly empty
    fieldDefn: $ => seq("var", $.id, PossiblyEmpty.optionalType($), optional(seq("as", optional("readonly"), $.id)), optional(seq("=", $.expression))),
    dotPathWord: $ => prec.right(seq($.idAll, repeat(seq(".", $.idAll)))),
    namespaceStatement: $ => seq($.dotPathWord, repeat(";")),
    usesStatementList: $ => seq("uses", $.usesStatement, repeat(seq("uses", $.usesStatement))),
    usesStatement: $ => seq($.dotPathWord, optional(seq(".", "*")), repeat(";")),
    typeVariableDefinition: $ => seq($.id, optional(seq("extends", $.typeLiteralList))),
    functionBody: $ => $.statementBlock,
    parameters: $ => seq("(", optional($.parameterDeclarationList), ")"),
    functionDefn: $ => seq("function", $.id, PossiblyEmpty.typeVariableDefs($), $.parameters, optional(seq(":", $.typeLiteral))),
    constructorDefn: $ => seq("construct", $.parameters, optional(seq(":", $.typeLiteral))),
    statement: $ => {
      // Gosu's grammar explicitly that statements may end with semicolons, but
      // also that a semicolon on its own is a statement. This is ambiguous
      // since it's unclear whether the following should be considered one or
      // two statements: `foo;`. Statements are parsed right-associatively so
      // that statements ending with semicolons are treated as single
      // statements rather than a pair of statements.
      return choice(prec.right(seq(choice(
        $.ifStatement,
        $.tryCatchFinallyStatement,
        $.throwStatement,
        "continue",
        "break",
        $.returnStatement,
        $.forEachStatement,
        $.whileStatement,
        $.doWhileStatement,
        $.switchStatement,
        $.usingStatement,
        $.assertStatement,
        seq("final", $.localVarStatement),
        $.localVarStatement,
        $.evalExpr,
        $.assignmentOrMethodCall,
        $.statementBlock,
      ), optional(";"))), ";");
    },
    ifStatement: $ => {
      // Right-associative so that the semicolon after the statement is considered part of the if-statement
      return prec.right(seq("if", "(", $.expression, ")", $.statement, optional(";"), optional(seq("else", $.statement))));
    },
    tryCatchFinallyStatement: $ => seq("try", $.statementBlock, choice(
      seq($.catchClause, repeat($.catchClause), optional(seq("finally", $.statementBlock))),
      seq("finally", $.statementBlock),
    )),
    catchClause: $ => seq("catch", "(", optional("var"), $.id, optional(seq(":", $.typeLiteral)), ")", $.statementBlock),
    assertStatement: $ => seq("assert", $.expression, optional(seq(":", $.expression))),
    usingStatement: $ => seq(
      "using",
      "(",
      choice(seq($.localVarStatement, repeat(seq(",", $.localVarStatement))), $.expression),
      ")",
      $.statementBlock,
      optional(seq("finally", $.statementBlock)),
    ),
    returnStatement: $ => prec.right(seq("return", optional($.expression))),
    whileStatement: $ => seq("while", "(", $.expression, ")", $.statement),
    doWhileStatement: $ => seq("do", $.statement, "while", "(", $.expression, ")"),
    switchStatement: $ => seq("switch", "(", $.expression, ")", "{", repeat($.switchBlockStatementGroup), "}"),
    switchBlockStatementGroup: $ => seq(choice(seq("case", $.expression, ":"), seq("default", ":")), $.statement),
    throwStatement: $ => seq("throw", $.expression),
    localVarStatement: $ => {
      // XXX check that it's correct for this to be right-associative
      return prec.right(seq("var", $.id, PossiblyEmpty.optionalType($), optional(seq("=", $.expression))));
    },
    forEachStatement: $ => seq(
      choice("foreach", "for"),
      "(",
      choice(seq($.expression, optional($.indexVar)), seq(optional("var"), $.id, "in", $.expression, optional($.indexRest))),
      ")",
      $.statement,
    ),
    indexRest: $ => choice(
      seq($.indexVar, $.iteratorVar),
      seq($.iteratorVar, $.indexVar),
      $.indexVar,
      $.iteratorVar,
    ),
    indexVar: $ => seq("index", $.id),
    iteratorVar: $ => seq("iterator", $.id),
    thisSuperExpr: _ => choice("this", "super"),
    assignmentOrMethodCall: $ => {
      // Without specifying an associativity for this rule the grammar
      // ambiguous. An example of the ambiguity is whether the following should
      // be interpreted as one or two statements: `foo(bar)`. Parenthesized
      // expressions (`parenthExpr`) are valid statements according to the
      // grammar, as are identifiers, and statement separators are entirely
      // optional. The choice of right associativity here treats the example
      // text as a single statement since otherwise function calls would not be
      // parsed as such.
      return prec.right(seq(
        choice($.typeLiteralExpr, $.parenthExpr, $.StringLiteral),
        PossiblyEmpty.indirectMemberAccess($),
        optional(choice(
          $.incrementOp,
          seq($.assignmentOp, $.expression),
        )),
      )); // XXX incomplete
    },
    statementBlock: $ => $.statementBlockBody,
    statementBlockBody: $ => seq("{", repeat($.statement), "}"),
    blockTypeLiteral: $ => $.blockLiteral,
    blockLiteral: $ => {
      // XXX check that it's correct for this to be right-associative
      return prec.right(seq("(", optional(seq($.blockLiteralArg, repeat(seq(",", $.blockLiteralArg)))), ")", optional(seq(":", $.typeLiteral))));
    },
    blockLiteralArg: $ => choice(
      seq($.id, optional(choice(seq("=", $.expression), $.blockTypeLiteral))),
      seq(optional(seq($.id, ":")), $.typeLiteral, optional(seq("=", $.expression))),
    ),
    typeLiteral: $ => {
      // XXX check that the "&" operator is indeed left-associative
      return prec.left(seq($.type, optional(seq("&", $.type))));
    },
    typeLiteralType: $ => $.typeLiteral,
    typeLiteralExpr: $ => $.typeLiteral,
    typeLiteralList: $ => $.typeLiteral,
    type: $ => choice(seq($.classOrInterfaceType, repeat(seq("[", "]"))), seq("block", $.blockLiteral)),
    classOrInterfaceType: $ => {
      // XXX is it correct that this is right-associative?
      return prec.right(seq($.idclassOrInterfaceType, PossiblyEmpty.typeArguments($), repeat(seq(".", $.id, PossiblyEmpty.typeArguments($)))));
    },
    // "typeArguments" is possibly empty
    typeArgument: $ => choice($.typeLiteralType, seq("?", optional(seq(choice("extends", "super"), $.typeLiteralType)))),
    expression: $ => $.conditionalExpr,
    conditionalExpr: $ => {
      // XXX check that the associativity is correct
      return prec.right(seq($.conditionalOrExpr, optional(choice(seq("?", $.conditionalExpr, ":", $.conditionalExpr), seq("?:", $.conditionalExpr)))));
    },
    conditionalOrExpr: $ => prec.left(seq($.conditionalAndExpr, repeat(seq($.orOp, $.conditionalAndExpr)))),
    conditionalAndExpr: $ => prec.left(seq($.bitwiseOrExpr, repeat(seq($.andOp, $.bitwiseOrExpr)))),
    bitwiseOrExpr: $ => prec.left(seq($.bitwiseXorExpr, repeat(seq("|", $.bitwiseXorExpr)))),
    bitwiseXorExpr: $ => prec.left(seq($.bitwiseAndExpr, repeat(seq("^", $.bitwiseAndExpr)))),
    bitwiseAndExpr: $ => prec.left(seq($.equalityExpr, repeat(seq("&", $.equalityExpr)))),
    equalityExpr: $ => prec.left(seq($.relationalExpr, repeat(seq($.equalityOp, $.relationalExpr)))),
    relationalExpr: $ => prec.left(seq($.intervalExpr, repeat(choice(seq($.relOp, $.intervalExpr), seq("typeis", $.typeLiteralType))))),
    intervalExpr: $ => prec.left(seq($.bitshiftExpr, optional(seq($.intervalOp, $.bitshiftExpr)))),
    bitshiftExpr: $ => prec.left(seq($.additiveExpr, repeat(seq($.bitshiftOp, $.additiveExpr)))),
    additiveExpr: $ => prec.left(seq($.multiplicativeExpr, repeat(seq($.additiveOp, $.multiplicativeExpr)))),
    multiplicativeExpr: $ => prec.left(seq($.typeAsExpr, repeat(seq($.multiplicativeOp, $.typeAsExpr)))),
    typeAsExpr: $ => prec.left(seq($.unaryExpr, repeat(seq($.typeAsOp, $.unaryExpr)))),
    unaryExpr: $ => choice(seq(choice("+", "-", "!-"), $.unaryExprNotPlusMinus), $.unaryExprNotPlusMinus),
    unaryExprNotPlusMinus: $ => choice(seq($.unaryOp, $.unaryExpr), seq("\\", $.blockExpr), $.evalExpr, $.primaryExpr),
    blockExpr: $ => seq(optional($.parameterDeclarationList), "->", choice($.expression, $.statementBlock)),
    evalExpr: $ => seq("eval", "(", $.expression, ")"),
    featureLiteral: $ => {
      // XXX check that it's correct for this to be right-associative
      return prec.right(seq("#", choice($.id, "construct"), PossiblyEmpty.typeArguments($), PossiblyEmpty.optionalArguments($)));
    },
    primaryExpr: $ => {
      // XXX check that it's correct for this to be right-associative
      return prec.right(seq(choice(
        $.literal,
        $.typeLiteralExpr,
        $.parenthExpr,
      ), PossiblyEmpty.indirectMemberAccess($))); // XXX incomplete
    },
    parenthExpr: $ => seq("(", $.expression, ")"),
    parameterDeclarationList: $ => seq($.parameterDeclaration, repeat(seq(",", $.parameterDeclaration))),
    parameterDeclaration: $ => seq(
      repeat($.annotation),
      optional("final"),
      $.id,
      optional(choice(
        seq(":", $.typeLiteral, optional(seq("=", $.expression))),
        $.blockTypeLiteral,
        seq("=", $.expression),
      )),
    ),
    annotationArguments: $ => $.arguments,
    // "optionArguments" is possibly empty
    arguments: $ => seq("(", optional(seq($.argExpression, repeat(seq(", ", $.argExpression)))), ")"),
    argExpression: $ => choice($.namedArgExpression, $.expression),
    namedArgExpression: $ => seq(":", $.id, "=", $.expression),
    literal: $ => choice($.NumberLiteral, $.featureLiteral, $.StringLiteral, $.CharLiteral, "true", "false", "null"),
    orOp: _ => choice("||", "or"),
    andOp: _ => choice("&&", "and"),
    assignmentOp: _ => choice("=", "+=", "-=", "*=", "/=", "&=", "&&=", "|=", "||=", "^=", "%=", seq("<", "<", "="), seq(">", ">", ">", "="), seq(">", ">", "=")),
    incrementOp: _ => choice("++", "--"),
    equalityOp: _ => choice("===", "!==", "==", "!="),
    intervalOp: _ => choice("..", "|..", "..|", "|..|"),
    relOp: _ => choice(seq("<", "="), seq(">", "="), "<", ">"),
    bitshiftOp: _ => choice(seq("<", "<"), seq(">", ">", ">"), seq(">", ">")),
    additiveOp: _ => choice("+", "-", "?+", "?-", "!+", "!-"),
    multiplicativeOp: _ => choice("*", "/", "%", "?*", "!*", "?/", "?%"),
    typeAsOp: _ => choice("typeas", "as"),
    unaryOp: _ => choice("~", "!", "not", "typeof", "statictypeof"),
    id: $ => {
      // Gosu's grammar explicitly matches keywords with this rule but keywords
      // are already naturally matched by "Ident".
      // XXX arbitrarily gave "id" precedence over "idclassOrInterfaceType". Need to check if this is correct.
      return prec(1, $.Ident);
    },
    idclassOrInterfaceType: $ => {
      // Gosu's grammar explicitly matches keywords with this rule but keywords
      // are already naturally matched by "Ident".
      // XXX is it correct that this is left associative?
      return prec.left($.Ident);
    },
    idAll: $ => {
      // Gosu's grammar explicitly matches keywords with this rule but keywords
      // are already naturally matched by "Ident".
      return $.Ident;
    },
    Ident: _ => {
      // Identifiers are defined as "tokens" which prevents them from being
      // split when an identifier happens to contain a keyword.
      return token(seq(Letter, repeat(choice(Digit, Letter))));
    },
    NumberLiteral: $ => choice("NaN", "Infinity", $.HexLiteral, $.BinLiteral, $.IntOrFloatPointLiteral),
    BinLiteral: $ => seq(choice("0b", "0B"), choice("0", "1"), repeat(choice("0", "1")), optional($.IntegerTypeSuffix)),
    HexLiteral: $ => seq(choice("0x", "0X"), $.HexDigit, repeat($.HexDigit), optional(choice("s", "S", "l", "L"))),
    IntOrFloatPointLiteral: $ => choice(
      seq(".", $.Digit, repeat($.Digit), optional($.FloatTypeSuffix)),
      seq($.Digit, repeat($.Digit),
        // In Gosu's grammar this is not explicitly optional but of the choices is the empty pattern which is equivalent but not allowed in tree-sitter.
        optional(
          choice(
            seq(".", $.Digit, repeat($.Digit), optional($.Exponent), optional($.FloatTypeSuffix)),
            seq($.Exponent, optional($.FloatTypeSuffix)),
            $.FloatTypeSuffix,
            $.IntegerTypeSuffix,
          ),
        ),
      ),
    ),
    CharLiteral: $ => {
      // In Gosu's syntax, char literals have the same syntax as
      // single-character single-quoted string literals. Give char literals
      // higher precedence to break the conflict.
      return  prec(1, seq("'", choice($.EscapeSequence, $.any_character), "'"));
    },
    StringLiteral: $ => choice(seq("'", repeat(choice($.EscapeSequence, $.any_character)), "'"), seq('"', repeat(choice($.EscapeSequence, $.any_character)), '"')),
    HexDigit: $ => choice($.Digit, /[A-F]/, /[a-f]/),
    IntegerTypeSuffix: _ => choice("l", "L", "s", "S", "bi", "BI", "b", "B"),
    Digit: _ => Digit,
    ZeroToSeven: _ => /[0-7]/,
    Exponent: $ => seq(choice("e", "E"), optional(choice("+", "-")), $.Digit, repeat($.Digit)),
    FloatTypeSuffix: _ => choice("f", "F", "d", "D", "bd", "BD"),
    EscapeSequence: $ => choice(seq("\\", choice("v", "a", "b", "t", "n", "f", "r", '"', "'", "\\", "$", "<")), $.UnicodeEscape, $.OctalEscape),
    OctalEscape: $ => choice(seq("\\", /0-3/, $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven)),
    UnicodeEscape: $ => seq("\\u", $.HexDigit, $.HexDigit, $.HexDigit, $.HexDigit),
    any_character: _ => /./,
    WS: _ => /\s/,
    COMMENT: _ => /\/\*([^\*]|(\*[^\/]))*\*\//,
    LINE_COMMENT: _ => /\/\/[^\n\r]*/,
  }
});
