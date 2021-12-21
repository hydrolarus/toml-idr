-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Value

import public Data.SortedMap
import Data.List
import Data.String

public export
Key : Type
Key = String

mutual
    public export
    data Value
        = VString String
        | VInteger Integer
        | VFloat Double
        | VBoolean Bool
        | VArray (List Value)
        | VTable Table
    
    public export
    Table : Type
    Table = SortedMap Key Value


export
lookupNested : (key : List String) -> (table : Table) -> Maybe Value
lookupNested [] table = Nothing
lookupNested [x] table = lookup x table
lookupNested (x :: xs) table = do
    VTable t <- lookup x table
        | _ => Nothing
    lookupNested xs t


public export
Eq Value where
    (VString x) == (VString y) = x == y
    (VInteger x) == (VInteger y) = x == y
    (VFloat x) == (VFloat y) = x == y
    (VBoolean x) == (VBoolean y) = x == y
    (VArray xs) == (VArray ys) = assert_total $ xs == ys
    (VTable x) == (VTable y) = assert_total $ SortedMap.toList x == SortedMap.toList y
    _ == _ = False

    a /= b = not $ (assert_total (==)) a b


public export
Show Value where
    show (VString x) = show x
    show (VInteger x) = show x
    show (VFloat x) = show x
    show (VBoolean True) = "true"
    show (VBoolean False) = "false"
    show (VArray xs) = "["
        ++ fastConcat (intersperse ", " $ map (assert_total show) xs)
        ++ "]"
    show (VTable x) =
        let kvs = the (List (Key, Value)) $ toList x in
        "{"
        ++ fastConcat (intersperse ", " . flip map kvs $ \(k, v) =>
                            show k ++ " = " ++ assert_total show v
                        )
        ++ "}"