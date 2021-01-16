-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Tests

import Test
import Test.Runner

import Parser
import Lexer
import Files

tests : List Test
tests = Lexer.tests
     ++ Parser.tests
     ++ Files.tests

main : IO ()
main = do
    success <- runTests Tests.tests
    if success
        then putStrLn "All tests passed."
        else putStrLn "Not all tests passed."