-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Lexer

import Text.Lexer

import Test

import Language.TOML.Lexer
import Language.TOML.Tokens

tokenize : Test
tokenize = test "tokenize" $ do
    Just res <- pure $ lexTOML "hello world\n"
        | Nothing => throwE "lexer error"
    assertEq (length res) 4

export
tests : List Test
tests = [tokenize]