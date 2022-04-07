-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Lexer

import Text.Lexer

import Tester

import Language.TOML.Lexer
import Language.TOML.Tokens

tokenize : Test
tokenize = test "tokenize" $ do
    let Just res = lexTOML "hello world\n"
        | Nothing => throwE "lexer error"
    assertEq (length res) 4

integerLiteral : Test
integerLiteral = test "lex integer literals" $ do
    let Just res = lexTOML "12345 12_345 0x12345 0x12_345"
        | Nothing => throwE "lexer error"
    assertEq (length res) 7

floatLiteral : Test
floatLiteral = test "lex float literals" $ do
    let Just res = lexTOML "5.0 0.55 0.0 -7.8e7 nan +nan -inf +inf"
        | Nothing => throwE "lexer error"
    assertEq (length res) 15

stringLiteral : Test
stringLiteral = test "lex string literals" $ do
    let Just res = lexTOML #""hello world""#
        | Nothing => throwE "lexer error"
    assertEq (length res) 1

export
tests : List Test
tests = [tokenize, integerLiteral, floatLiteral, stringLiteral]
