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
  optionalType: $ => optional(seq(":", $.typeLiteral)), // XXX incomplete
  indirectMemberAccess: $ => repeat(choice($.arguments)), // XXX incomplete
  typeVariableDefs: $ => optional(seq("<", $.typeVariableDefinition, repeat(seq(",", $.typeVariableDefinition)), ">")),
  typeArguments: $ => optional(seq("<", $.typeArgument, repeat(seq(",", $.typeArgument)), ">")),
  classMembers: $ => repeat($.declaration),
  optionalArguments: $ => optional($.arguments),
};

module.exports = grammar({
  name: "gosu",

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
    declaration: $ => seq(PossiblyEmpty.modifiers($), choice($.fieldDefn), optional(";")), // XXX incomplete
    // "optionalType" is possibly empty
    fieldDefn: $ => seq("var", $.id, PossiblyEmpty.optionalType($), optional(seq("as", optional("readonly"), $.id)), optional(seq("=", $.expression))),
    dotPathWord: $ => prec.right(seq($.idAll, repeat(seq(".", $.idAll)))),
    namespaceStatement: $ => seq($.dotPathWord, repeat(";")),
    usesStatementList: $ => seq("uses", $.usesStatement, repeat(seq("uses", $.usesStatement))),
    usesStatement: $ => seq($.dotPathWord, optional(seq(".", "*")), repeat(";")),
    typeVariableDefinition: $ => seq($.id, optional(seq("extends", $.typeLiteralList))),
    statement: $ => {
      // Gosu's grammar explicitly that statements may end with semicolons, but
      // also that a semicolon on its own is a statement. This is ambiguous
      // since it's unclear whether the following should be considered one or
      // two statements: `foo;`. Statements are parsed right-associatively so
      // that statements ending with semicolons are treated as single
      // statements rather than a pair of statements.
      return choice(prec.right(seq(choice($.localVarStatement, $.assignmentOrMethodCall), optional(";"))), ";"); // XXX incomplete
    },
    localVarStatement: $ => seq("var", $.id, PossiblyEmpty.optionalType($), optional(seq("=", $.expression))),
    assignmentOrMethodCall: $ => {
      // Without specifying an associativity for this rule the grammar
      // ambiguous. An example of the ambiguity is whether the following should
      // be interpreted as one or two statements: `foo(bar)`. Parenthesized
      // expressions (`parenthExpr`) are valid statements according to the
      // grammar, as are identifiers, and statement separators are entirely
      // optional. The choice of right associativity here treats the example
      // text as a single statement since otherwise function calls would not be
      // parsed as such.
      return prec.right(seq(choice($.typeLiteralExpr, $.parenthExpr, $.StringLiteral), PossiblyEmpty.indirectMemberAccess($))); // XXX incomplete
    },
    typeLiteral: $ => seq($.type, optional(seq("&", $.type))),
    typeLiteralType: $ => $.typeLiteral,
    typeLiteralExpr: $ => $.typeLiteral,
    typeLiteralList: $ => $.typeLiteral,
    type: $ => $.classOrInterfaceType, // XXX incomplete
    classOrInterfaceType: $ => seq($.idclassOrInterfaceType, PossiblyEmpty.typeArguments($), repeat(seq(".", $.id, PossiblyEmpty.typeArguments($)))),
    // "typeArguments" is possibly empty
    typeArgument: $ => choice($.typeLiteralType, seq("?", optional(seq(choice("extends", "super"), $.typeLiteralType)))),
    expression: $ => $.conditionalExpr,
    conditionalExpr: $ => $.conditionalOrExpr, // XXX incomplete
    conditionalOrExpr: $ => $.conditionalAndExpr, // XXX incomplete
    conditionalAndExpr: $ => $.bitwiseOrExpr, // XXX incomplete
    bitwiseOrExpr: $ => $.bitwiseXorExpr, // XXX incomplete
    bitwiseXorExpr: $ => $.bitwiseAndExpr, // XXX incomplete
    bitwiseAndExpr: $ => $.equalityExpr, // XXX incomplete
    equalityExpr: $ => $.relationalExpr, // XXX incomplete
    relationalExpr: $ => $.intervalExpr, // XXX incomplete
    intervalExpr: $ => $.bitshiftExpr, // XXX incomplete
    bitshiftExpr: $ => $.additiveExpr, // XXX incomplete
    additiveExpr: $ => $.multiplicativeExpr, // XXX incomplete
    multiplicativeExpr: $ => $.typeAsExpr, // XXX incomplete
    typeAsExpr: $ => $.unaryExpr, // XXX incomplete
    unaryExpr: $ => $.unaryExprNotPlusMinus, // XXX incomplete
    unaryExprNotPlusMinus: $ => $.primaryExpr, // XXX incomplete
    featureLiteral: $ => seq("#", choice($.id, "construct"), PossiblyEmpty.typeArguments($), PossiblyEmpty.optionalArguments($)),
    primaryExpr: $ => choice($.literal, $.typeLiteralExpr), // XXX incomplete
    parenthExpr: $ => seq("(", $.expression, ")"),
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
      return $.Ident;
    },
    idclassOrInterfaceType: $ => {
      // Gosu's grammar explicitly matches keywords with this rule but keywords
      // are already naturally matched by "Ident".
      return $.Ident;
    },
    idAll: $ => {
      // Gosu's grammar explicitly matches keywords with this rule but keywords
      // are already naturally matched by "Ident".
      return $.Ident;
    },
    Ident: $ => {
      // Right associative so parsing identifiers ends as late as possible.
      return prec.right(seq($.Letter, repeat(choice($.Digit, $.Letter))));
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
    Letter: _ => /[A-Za-z_$]/,
    Digit: _ => /[0-9]/,
    ZeroToSeven: _ => /[0-7]/,
    Exponent: $ => seq(choice("e", "E"), optional(choice("+", "-")), $.Digit, repeat($.Digit)),
    FloatTypeSuffix: _ => choice("f", "F", "d", "D", "bd", "BD"),
    EscapeSequence: $ => choice(seq("\\", choice("v", "a", "b", "t", "n", "f", "r", '"', "'", "\\", "$", "<")), $.UnicodeEscape, $.OctalEscape),
    OctalEscape: $ => choice(seq("\\", /0-3/, $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven)),
    UnicodeEscape: $ => seq("\\u", $.HexDigit, $.HexDigit, $.HexDigit, $.HexDigit),
    any_character: _ => /./,
  }
});
