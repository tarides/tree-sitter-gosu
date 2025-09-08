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
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
