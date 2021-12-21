-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML


import Data.List
import Data.List1
import Data.SortedMap

import Text.Bounded
import Text.Token
import Language.TOML.Lexer
import Language.TOML.Parser
import public Language.TOML.Value
import Language.TOML.ConcreteSyntax as C



private
data SectionIdent = SGlobal
                  | STable CKey
                  | STableArray CKey

private
sections : List C.Item -> List (SectionIdent, List (CKey, CValue))
sections xs = loop SGlobal xs
    where
        loop : SectionIdent -> List C.Item -> List (SectionIdent, List (CKey, CValue))
        loop sec [] = []
        loop sec ((ITableHeader header) :: xs) = loop (STable header) xs
        loop sec ((ITableArray header) :: xs) = loop (STableArray header) xs
        loop sec xs =
            let (kvs', rest) = flip break xs $ \x => case x of
                                        IKeyValue k v => False
                                        _ => True
                kvs = flip mapMaybe kvs' $ \x => case x of
                                        IKeyValue k v => Just (k, v)
                                        _ => Nothing
                
            in (sec, kvs) :: loop sec rest


public export
data Error = ErrDottedIsNotATable Key Value
           | LexerError
           | ParseError (List String)
           | Unimplemented


public export
Show Error where
    show (ErrDottedIsNotATable x y) = "Dotted key part `" ++ show x ++ "`is not a table"
    show LexerError = "Lexer error"
    show (ParseError x) = "Parse error: " ++ show x
    show Unimplemented = "Unimplemented feature"


private
keyAtomStr : CKeyAtom -> Key
keyAtomStr (CKBare x) = x
keyAtomStr (CKQuoted x) = x

private
keyParts : CKey -> List1 Key
keyParts (CKAtom x) = keyAtomStr x ::: []
keyParts (CKDotted x) = map keyAtomStr x


private
tableSetWithParts : (t : Table) -> (path : List1 Key) -> (val : Value) -> Either Error Table
tableSetWithParts t (head ::: []) val = pure $ insert head val t
tableSetWithParts t (head ::: (x :: xs)) val = do
    inner <- case lookup head t of
                    Nothing => pure empty
                    Just (VTable t) => pure t
                    Just v => Left $ ErrDottedIsNotATable head v
    
    inner' <- tableSetWithParts inner (x ::: xs) val
    pure $ insert head (VTable inner') t


mutual

    private
    cvalToVal : CValue -> Either Error Value
    cvalToVal (CVString x) = pure $ VString x
    cvalToVal (CVInteger x) = pure $ VInteger x
    cvalToVal (CVFloat x) = pure $ VFloat x
    cvalToVal (CVBoolean x) = pure $ VBoolean x
    cvalToVal (CVArray xs) = map VArray $ for xs cvalToVal
    cvalToVal (CVInlineTable xs) = map VTable $ tableFromKVs xs

    private
    tableFromKVs : List (CKey, CValue) -> Either Error Table
    tableFromKVs xs = loop empty xs
        where
            loop : Table -> List (CKey, CValue) -> Either Error Table
            loop t [] = Right t
            loop t ((k, v) :: xs) = do
                v' <- cvalToVal v
                let parts = keyParts k
                t' <- tableSetWithParts t parts v'
                loop t' xs
    

private
extendFile : (file : Table) -> (sects : List (SectionIdent, List (CKey, CValue))) -> Either Error Table
extendFile file [] = pure file
extendFile file ((SGlobal, kvs) :: rest) = do
    tab <- tableFromKVs kvs
    extendFile (mergeLeft tab file) rest
extendFile file (((STable key), kvs) :: rest) = do
    tab <- tableFromKVs kvs
    let kParts = keyParts key
    file' <- tableSetWithParts file kParts (VTable tab)
    extendFile file' rest

extendFile file (((STableArray x), kvs) :: rest) = Left Unimplemented


export
parseTOML : (src : String) -> Either Error Table
parseTOML src = do
    let Just toks = lexTOML src
        | Nothing => Left LexerError

    items <- bimap ParseError id (parseItems toks)

    let sects = sections items

    extendFile empty sects