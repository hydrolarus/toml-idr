-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.ConcreteSyntax

import Data.Strings
import Data.List
import Data.List1

public export
data CKeyAtom = CKBare String
              | CKQuoted String

public export
data CKey = CKAtom CKeyAtom
          | CKDotted (List1 CKeyAtom)

public export
data CValue
    = CVString String
    | CVInteger Integer
    | CVFloat Double
    | CVBoolean Bool
    | CVArray (List CValue)
    | CVInlineTable (List (CKey, CValue))


public export
data Item : Type where
    IKeyValue : (key : CKey) -> (val : CValue) -> Item
    ITableHeader : (header : CKey) -> Item
    ITableArray : (header : CKey) -> Item


public export
Show CKeyAtom where
    show (CKBare x) = x
    show (CKQuoted x) = "\"" ++ x ++ "\""

public export
Show CKey where
    show (CKAtom x) = show x
    show (CKDotted x) = fastConcat . intersperse "." . forget $ show <$> x

public export
Show CValue where
    show (CVString x) = show x
    show (CVInteger x) = show x
    show (CVFloat x) = show x
    show (CVBoolean x) = show x
    show (CVArray xs) = "["
        ++ fastConcat (intersperse ", " $ map show xs)
        ++ "]"
    show (CVInlineTable xs) = "{"
        ++ fastConcat (intersperse ", " $ map (\(k, v) => show k ++ " = " ++ show v) xs)
        ++ "}"

public export
Show Item where
    show (IKeyValue key val) = "keyval: " ++ show key ++ " = " ++ show val
    show (ITableHeader header) = "table: " ++ show header
    show (ITableArray header) = "tablearr: " ++ show header