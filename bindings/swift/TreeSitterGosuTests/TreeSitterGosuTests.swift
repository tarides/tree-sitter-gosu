import XCTest
import SwiftTreeSitter
import TreeSitterGosu

final class TreeSitterGosuTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_gosu())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Gosu grammar")
    }
}
