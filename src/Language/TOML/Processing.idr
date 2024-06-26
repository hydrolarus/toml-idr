-- SPDX-FileCopyrightText: 2022 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Processing

import Data.List.Elem
import Decidable.Equality
import Language.TOML.Value

export infixr 2 `And`

mutual
    public export
    data ValueTy
        = TString
        | TInteger
        | TFloat
        | TBoolean
        | TEnum (List String)
        | TArray ValueTy
        | TTable TableTy

    public export
    record FieldTy where
        constructor MkFieldTy
        name : String
        optional : Bool
        ty : ValueTy

    public export
    data TableTy : Type where
        Emp : TableTy
        Ext : (ft : FieldTy) -> (FieldOf ft -> TableTy) -> TableTy

    public export
    And : FieldTy -> TableTy -> TableTy
    And x y = Ext x (const y)

    public export
    ValueOf : ValueTy -> Type
    ValueOf TString    = String
    ValueOf TInteger   = Integer
    ValueOf TFloat     = Double
    ValueOf TBoolean   = Bool
    ValueOf (TEnum names) = (name ** Elem name names)
    ValueOf (TArray t) = List (ValueOf t)
    ValueOf (TTable x) = TableOf x

    public export
    FieldOf : FieldTy -> Type
    FieldOf x = (if x.optional then Maybe else id) (ValueOf x.ty)

    public export
    data TableOf : TableTy -> Type where
        Nil : TableOf Emp
        (::) : (x : FieldOf ft) -> TableOf (f x) -> TableOf (Ext ft f)


mutual
    public export
    data ValueError
        = ExpectedType ValueTy
        | InsideArray ValueError
        | InsideTable TableError
        | BadEnumVal String

    public export
    data TableError
        = FieldError String ValueError
        | ExpectedField String
        | UnexpectedFields (List String)


mutual
    public export
    processTable : (ty : TableTy) -> Table -> Either TableError (TableOf ty)
    processTable Emp x with (keys x)
      _ | [] = Right []
      _ | ks = Left $ UnexpectedFields ks
    processTable (Ext (MkFieldTy name optional ty) fields) x with (lookup name x)
      _ | Nothing = case optional of
                         True => Right $ Nothing :: !(processTable (fields Nothing) x)
                         False => Left $ ExpectedField name
      _ | Just val = do
              val' <- bimap (FieldError name) id $ processValue ty val
              let val'' = case optional of
                               True => Just val'
                               False => val'
              fields' <- processTable (fields val'') (delete name x)
              Right (val'' :: fields')

    public export
    processValue : (ty : ValueTy) -> Value -> Either ValueError (ValueOf ty)
    processValue (TEnum xs) (VString x) with (isElem x xs)
      _ | Yes el = Right (x ** el)
      _ | No nel = Left $ BadEnumVal x
    processValue TString    (VString x)  = Right x
    processValue TInteger   (VInteger x) = Right x
    processValue TFloat     (VFloat x)   = Right x
    processValue TBoolean   (VBoolean x) = Right x
    processValue (TArray t) (VArray xs)  = traverse (bimap InsideArray id . processValue t) xs
    processValue (TTable t) (VTable x)   = bimap InsideTable id $ processTable t x
    processValue t          v            = Left $ ExpectedType t
