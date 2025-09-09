/**
 * @file Gosu is a statically typed general-purpose programming language that runs on the Java Virtual Machine.
 * @author Marek Kubica <marek@tarides.com>
 * @license UNLICENSED
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "gosu",

  rules: {
    // TODO recurse here
    id: $ => seq("a"),

    dot_path_word: $ => seq(id, repeat(".", id)),
    uses_statement: $ => seq(dot_path_word, optional(".*"), optional(repeat(";"))),
    uses_statement_list: $ => seq("uses", uses_statement, repeat(seq("uses", uses_statement))),
    header: $ => seq(optional(seq("package")), uses_statement_list),

    // TODO: rest of start
    start: $ => header,

    // TODO: add the actual grammar rules
    source_file: $ => start
  }
});
