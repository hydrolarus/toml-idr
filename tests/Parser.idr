-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Parser

import Test
import Control.ANSI

import Text.Lexer

import Language.TOML
import Language.TOML.Tokens
import Language.TOML.Parser as P
import Language.TOML.Lexer as L
import Language.TOML.ConcreteSyntax
import Language.TOML.Value

parseToItems : String -> TestFunc (List Item)
parseToItems src = do
    Just toks <- pure $ L.lexTOML src
        | Nothing => throw "lexing failed"
    Right items <- pure $ P.parseItems toks
        | Left err => throw (show (colored Red "parsing failed: ") ++ err)
    pure items


parse : String -> TestFunc Table
parse src = do
    Right table <- pure $ parseTOML src
        | Left err => throw (show err)
    pure table

namespace Concrete
    export
    kvNoNewline : Test
    kvNoNewline = test "parse key-value no newline" $ do
        items <- parseToItems "x = 12"
        assertEq (show items) "[keyval: x = 12]"

    export
    kvComment : Test
    kvComment = test "parse key-value with comment" $ do
        items <- parseToItems "x = 12 # this is a key value pair"
        assertEq (show items) "[keyval: x = 12]"

    export
    kvNewline : Test
    kvNewline = test "parse key-value with newline" $ do
        items <- parseToItems "x = 12\n"
        assertEq (show items) "[keyval: x = 12]"

    export
    tableHeader : Test
    tableHeader = test "parse table header" $ do
        items <- parseToItems "[package]"
        assertEq (show items) "[table: package]"

    export
    tableHeaderDotted : Test
    tableHeaderDotted = test "parse table header with dotted key" $ do
        items <- parseToItems "[dependencies.test]"
        assertEq (show items) "[table: dependencies.test]"

    export
    tableArray : Test
    tableArray = test "parse table array header" $ do
        items <- parseToItems "[[bin]]"
        assertEq (show items) "[tablearr: bin]"

    export
    tableArrayDotted : Test
    tableArrayDotted = test "parse table array header with dotted key" $ do
        items <- parseToItems "[[bin.tags]]"
        assertEq (show items) "[tablearr: bin.tags]"


    export
    emptyFile : Test
    emptyFile = test "parse empty file" $ do
        items <- parseToItems ""
        assertEq (show items) "[]"

    export
    kvArrayNewLine : Test
    kvArrayNewLine = test "key-val with array with newlines" $ do
        items <- parseToItems "x = [\n\t1,\n\t2,\n\t3\n]"
        assertEq (show items) "[keyval: x = [1, 2, 3]]"

    export
    kvInlineTable : Test
    kvInlineTable = test "key-val with inline table" $ do
        items <- parseToItems "test = { path = \"../test-idr\" }"
        -- TODO strings are not interspreted *and* are `show`n
        assertEq (show items) "[keyval: test = {path = \"../test-idr\"}]"




namespace Abstract
    export
    kvGlobal : Test
    kvGlobal = test "global key-val" $ do
        table <- parse "x = 12"
        assertEq [("x", VInteger 12)] (toList table)
    
    export
    kvNested : Test
    kvNested = test "global nested key-val" $ do
        table <- parse "x.y = 12"
        assertEq (lookupNested ["x", "y"] table) (Just $ VInteger 12)

    export
    tableSectionKv : Test
    tableSectionKv = test "table section key-val" $ do
        table <- parse "[table-name]\nx = 12"
        assertEq (lookupNested ["table-name", "x"] table) (Just $ VInteger 12)

    export
    tableSectionNestedKv : Test
    tableSectionNestedKv = test "table section nested key-val" $ do
        table <- parse "[table-name]\nx.y = 12"
        assertEq (lookupNested ["table-name", "x", "y"] table) (Just $ VInteger 12)

    export
    inlineTable : Test
    inlineTable = test "inline table" $ do
        table <- parse "test = { x = 12, y = 21 }"
        assertEq (lookupNested ["test", "x"] table) (Just $ VInteger 12)
    
    export
    array : Test
    array = test "array" $ do
        table <- parse "x = [1, 3, 7]"
        assertEq (lookup "x" table) (Just $ VArray [VInteger 1, VInteger 3, VInteger 7])



public export
tests : List Test
tests = [
    Concrete.kvNoNewline,
    Concrete.kvComment,
    Concrete.kvNewline,
    Concrete.tableHeader,
    Concrete.tableHeaderDotted,
    Concrete.tableArray,
    Concrete.tableArrayDotted,
    Concrete.emptyFile,
    Concrete.kvArrayNewLine,
    Concrete.kvInlineTable,

    Abstract.kvGlobal,
    Abstract.kvNested,
    Abstract.tableSectionKv,
    Abstract.tableSectionNestedKv,
    Abstract.inlineTable,
    Abstract.array
]