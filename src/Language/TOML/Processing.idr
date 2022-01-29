-- SPDX-FileCopyrightText: 2022 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Processing

import Data.List.Quantifiers
import Language.TOML.Value

-- On Idris 0.5.2 release, remove this in favour of using Data.Singleton.
mutual
    public export
    data ValueTy
        = TString
        | TInteger
        | TFloat
        | TBoolean
        | TArray ValueTy
        | TTable TableTy

    public export
    record FieldTy where
        constructor MkFieldTy
        name : String
        optional : Bool
        ty : ValueTy

    public export
    TableTy : Type
    TableTy = List FieldTy


mutual
    public export
    ValueOf : ValueTy -> Type
    ValueOf TString    = String
    ValueOf TInteger   = Integer
    ValueOf TFloat     = Double
    ValueOf TBoolean   = Bool
    ValueOf (TArray t) = List (ValueOf t)
    ValueOf (TTable x) = TableOf x

    public export
    FieldOf : FieldTy -> Type
    FieldOf x = (if x.optional then Maybe else id) (ValueOf x.ty)

    public export
    TableOf : TableTy -> Type
    TableOf = All FieldOf


mutual
    public export
    data ValueError
        = ExpectedType ValueTy
        | InsideArray ValueError
        | InsideTable TableError

    public export
    data TableError
        = FieldError String ValueError
        | ExpectedField String
        | UnexpectedFields (List String)


mutual
    public export
    processTable : (ty : TableTy) -> Table -> Either TableError (TableOf ty)
    processTable [] x with (keys x)
      _ | [] = Right []
      _ | ks = Left $ UnexpectedFields ks
    processTable (MkFieldTy name optional ty :: fields) x with (lookup name x)
      _ | Nothing = case optional of
                         True => Right $ Nothing :: !(processTable fields x)
                         False => Left $ ExpectedField name
      _ | Just val = do
              val' <- bimap (FieldError name) id $ processValue ty val
              fields' <- processTable fields (delete name x)
              let val'' = case optional of
                               True => Just val'
                               False => val'
              Right (val'' :: fields')

    public export
    processValue : (ty : ValueTy) -> Value -> Either ValueError (ValueOf ty)
    processValue TString    (VString x)  = Right x
    processValue TInteger   (VInteger x) = Right x
    processValue TFloat     (VFloat x)   = Right x
    processValue TBoolean   (VBoolean x) = Right x
    processValue (TArray t) (VArray xs)  = traverse (bimap InsideArray id . processValue t) xs
    processValue (TTable t) (VTable x)   = bimap InsideTable id $ processTable t x
    processValue t          v            = Left $ ExpectedType t
