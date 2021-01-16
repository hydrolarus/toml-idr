-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Files

import Test

import Language.TOML

import System.Directory
import System.File

parse : String -> TestFunc Table
parse src = do
    Right table <- pure $ parseTOML src
        | Left err => throw (show err)
    pure table

dirEntries : (path : String) -> TestFunc (List String)
dirEntries path = do
    Right dir <- openDir path
        | Left err =>
            throw $ "\nUnable to open dir \"" ++ path ++ "\": " ++ show err
    entries <- listEntries dir
    closeDir dir
    pure entries
    where
        listEntries : Directory -> TestFunc (List String)
        listEntries dir = do
            Right s <- dirEntry dir
                | Left (GenericFileError 22) => pure []
                | Left err => do
                    throw $ show err
            
            rest <- listEntries dir
            case s of
                "." => pure rest
                ".." => pure rest
                _ => pure $ s :: rest
    


filesPass : Test
filesPass = test "valid files" $ do
    let basePath = "tests/pass/"
    dirs <- dirEntries basePath
    for_ dirs \path => do
        Right src <- readFile (basePath ++ path)
            | Left err => throw $ show err
        
        Right _ <- pure $ parseTOML src
            | Left err => throw $ show err
        
        pure ()
        

export
tests : List Test
tests = [
    filesPass
]