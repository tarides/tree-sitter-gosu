/**
 * @file Gosu is a statically typed general-purpose programming language that runs on the Java Virtual Machine.
 * @author Marek Kubica <marek@tarides.com>
 * @license UNLICENSED
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "gosu",

  // Based on the grammer at: https://gosu-lang.github.io/grammar.html
  rules: {
    source_file: $ => repeat($.statement),
    typeAnnotation: $ => {
      // Gosu's grammar refers to this rule `optionalType`, and permits the
      // rule to match the empty string indicating the absence of a type
      // annotation. Tree-sitter does not permit rules to match the empty
      // string, so Gosu's tree-sitter grammer must be changed to make type
      // annotations optional at the call sites (ie. `optional($.typeAnnotation)`).
      return seq(":", $.typeLiteral); // XXX incomplete
    },
    statement: $ => {
      // Gosu's grammar explicitly that statements may end with semicolons, but
      // also that a semicolon on its own is a statement. This is ambiguous
      // since it's unclear whether the following should be considered one or
      // two statements: `foo;`. Statements are parsed right-associatively so
      // that statements ending with semicolons are treated as single
      // statements rather than a pair of statements.
      return choice(prec.right(seq(choice($.localVarStatement, $.assignmentOrMethodCall), optional(";"))), ";"); // XXX incomplete
    },
    localVarStatement: $ => seq("var", $.id, optional($.typeAnnotation), optional(seq("=", $.expression))),
    assignmentOrMethodCall: $ => {
      // XXX Without specifying an associativity for this rule the grammar
      // ambiguous. An example of the ambiguity is whether the following should
      // be interpreted as one or two statements: `foo(bar)`. Parenthesized
      // expressions (`parenthExpr`) are valid statements according to the
      // grammar, as are identifiers, and statement separators are entirely
      // optional. The choice of right associativity here treats the example
      // text as a single statement since otherwise function calls would not be
      // parsed as such.
      return prec.right(seq(choice($.typeLiteralExpr, $.parenthExpr, $.StringLiteral), repeat($.indirectMemberAccessOne))); // XXX incomplete
    },
    classOrInterfaceType: $ => $.idclassOrInterfaceType, // XXX incomplete
    type: $ => $.classOrInterfaceType, // XXX incomplete
    typeLiteral: $ => seq($.type, optional(seq("&", $.type))),
    typeLiteralExpr: $ => $.typeLiteral,
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
    primaryExpr: $ => choice($.literal, $.typeLiteralExpr), // XXX incomplete
    parenthExpr: $ => seq("(", $.expression, ")"),
    arguments: $ => seq("(", optional(seq($.argExpression, repeat(seq(", ", $.argExpression)))), ")"),
    argExpression: $ => choice($.namedArgExpression, $.expression),
    namedArgExpression: $ => seq(":", $.id, "=", $.expression),
    indirectMemberAccessOne: $ => {
      // Gosu's grammer refers to this rule as `indirectMemberAccess` and
      // allows matching the inner pattern 0 or more times. Since this would
      // match the empty string, this is not allowed in a tree-sitter grammer.
      // Instead the repetition must be introduced at the call site.
      return choice($.arguments); // XXX incomplete
    },
    literal: $ => $.StringLiteral, // XXX incomplete
    idclassOrInterfaceType: $ => $.Ident,
    id: $ => $.Ident,
    Ident: $ => {
      // Right associative so parsing identifiers ends as late as possible.
      return prec.right(seq($.Letter, repeat(choice($.Digit, $.Letter))));
    },
    StringLiteral: $ => choice(seq("'", repeat(choice($.EscapeSequence, $.any_character)), "'"), seq('"', repeat(choice($.EscapeSequence, $.any_character)), '"')),
    HexDigit: $ => choice($.Digit, /[A-F]/, /[a-f]/),
    Letter: _ => /[A-Za-z_$]/,
    Digit: _ => /[0-9]/,
    ZeroToSeven: _ => /[0-7]/,
    EscapeSequence: $ => choice(seq("\\", choice("v", "a", "b", "t", "n", "f", "r", '"', "'", "\\", "$", "<")), $.UnicodeEscape, $.OctalEscape),
    OctalEscape: $ => choice(seq("\\", /0-3/, $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven, $.ZeroToSeven), seq("\\", $.ZeroToSeven)),
    UnicodeEscape: $ => seq("\\u", $.HexDigit, $.HexDigit, $.HexDigit, $.HexDigit),
    any_character: _ => /./,
  }
});
