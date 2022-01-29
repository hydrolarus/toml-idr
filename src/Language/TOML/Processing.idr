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
    TableTy : Type
    TableTy = List (String, ValueTy)


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
    TableOf : TableTy -> Type
    TableOf = All (ValueOf . snd)


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
    processTable ((name, ty) :: fields) x with (lookup name x)
      _ | Nothing = Left $ ExpectedField name
      _ | Just val = do
              val' <- bimap (FieldError name) id $ processValue ty val
              fields' <- processTable fields (delete name x)
              Right (val' :: fields')

    public export
    processValue : (ty : ValueTy) -> Value -> Either ValueError (ValueOf ty)
    processValue TString    (VString x)  = Right x
    processValue TInteger   (VInteger x) = Right x
    processValue TFloat     (VFloat x)   = Right x
    processValue TBoolean   (VBoolean x) = Right x
    processValue (TArray t) (VArray xs)  = traverse (bimap InsideArray id . processValue t) xs
    processValue (TTable t) (VTable x)   = bimap InsideTable id $ processTable t x
    processValue t          v            = Left $ ExpectedType t
