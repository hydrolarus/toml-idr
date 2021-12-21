-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Files

import Tester

import Language.TOML


import Data.List
import System.Directory
import System.File


parse : String -> TestFunc Table
parse src = do
    Right table <- pure $ parseTOML src
        | Left err => throw (show err)
    pure table

dirEntries : (path : String) -> TestFunc (List String)
dirEntries path = do

    Right contents <- listDir path
        | Left err =>
            throw $ "\nUnable to open dir \"" ++ path ++ "\": " ++ show err

    pure $ filter (\x => not $ x `elem` [".", ".."]) contents
    


filesPass : Test
filesPass = test "valid files" $ do
    let basePath = "tests/pass/"
    dirs <- dirEntries basePath
    for_ dirs $ \path => do
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