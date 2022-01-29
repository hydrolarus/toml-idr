-- SPDX-FileCopyrightText: 2022 The toml-idr developers
--
-- SPDX-License-Identifier: CC0-1.0

module Process

import Tester

import Data.List.Quantifiers
import Language.TOML
import Language.TOML.Processing
import Data.List.Elem

basicUnordered : Test
basicUnordered = test "process unordered table" $ assert $ check result
    where
        Spec : TableTy
        Spec = MkFieldTy "name" False TString `And` MkFieldTy "age" False TInteger `And` Emp

        table : Table
        table = fromList [("age", VInteger 999), ("name", VString "John Doe")]

        result : Either TableError (TableOf Spec)
        result = processTable Spec table

        check : Either TableError (TableOf Spec) -> Bool
        check (Left x) = False
        check (Right [name, age]) = name == "John Doe" && age == 999

basicNested : Test
basicNested = test "process nested table" $ assert $ check result
    where
        Spec : TableTy
        Spec = MkFieldTy "name" False TString `And`
                MkFieldTy "age" False TInteger `And`
                MkFieldTy "skills" False (TTable
                    (MkFieldTy "running" False TInteger `And`
                     MkFieldTy "programming" False TInteger `And`
                     Emp)) `And`
                Emp

        table : Table
        table = fromList [("age", VInteger 999),
                          ("name", VString "John Doe"),
                          ("skills", VTable $ fromList
                              [("running", VInteger 5),
                               ("programming", VInteger 8)])]

        result : Either TableError (TableOf Spec)
        result = processTable Spec table

        check : Either TableError (TableOf Spec) -> Bool
        check (Left x) = False
        check (Right [name, age, [running, programming]]) =
            name == "John Doe" && age == 999 && running == 5 && programming == 8

basicOptional : Test
basicOptional = test "process table with optional field" $ assert $ check result
    where
        Spec : TableTy
        Spec = MkFieldTy "name" True TString `And` MkFieldTy "age" False TInteger `And` Emp

        table : Table
        table = fromList [("age", VInteger 999)]

        result : Either TableError (TableOf Spec)
        result = processTable Spec table

        check : Either TableError (TableOf Spec) -> Bool
        check (Left x) = False
        check (Right [name, age]) = name == Nothing && age == 999

basicDependent : Test
basicDependent = test "process table dependent layout" $ assert $ check result
    where
        Spec : TableTy
        Spec = Ext (MkFieldTy "programmer" False TBoolean) $ \programmer =>
            if programmer
               then MkFieldTy "fave-lang" False TString `And` Emp
               else Emp

        table : Table
        table = fromList [("programmer", VBoolean True), ("fave-lang", VString "idris")]

        result : Either TableError (TableOf Spec)
        result = processTable Spec table

        check : Either TableError (TableOf Spec) -> Bool
        check (Left x) = False
        check (Right (True :: lang :: [])) = lang == "idris"
        check (Right (False :: tbl)) = False


basicEnum : Test
basicEnum = test "process enum" $ assert $ check result
    where
        Spec : ValueTy
        Spec = TEnum ["idris", "haskell", "lisp"]

        table : Value
        table = VString "idris"

        result : Either ValueError (ValueOf Spec)
        result = processValue Spec table

        check : Either ValueError (ValueOf Spec) -> Bool
        check (Left x) = False
        check (Right ("idris" ** _)) = True
        check (Right (_ ** _)) = False

export
tests : List Test
tests = [basicUnordered, basicNested, basicOptional, basicDependent, basicEnum]
