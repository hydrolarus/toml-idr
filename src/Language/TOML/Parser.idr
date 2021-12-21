-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Parser

import Language.TOML.ConcreteSyntax
import Language.TOML.Tokens

import Text.Parser
import Text.Token

import Data.List
import Data.List1

private
punct : Punctuation -> Grammar state TOMLToken True ()
punct p = match $ TTPunct p

private
maybeNewlines : Grammar state TOMLToken False ()
maybeNewlines = do
    _ <- many (punct NewLine)
    pure ()

private
newlines : Grammar state TOMLToken False ()
newlines = (some (punct NewLine) >>= \_ => pure ()) <|> eof

private
allowNewlines : (p : Grammar state TOMLToken True a) -> Grammar state TOMLToken True a
allowNewlines p = maybeNewlines *> p <* maybeNewlines


private
unescape : List Char -> List Char
unescape ('"'::rest) = loop rest
    where
        loop : List Char -> List Char
        loop [] = []
        loop ('"'::_) = []
        loop ('\\'::'"'::rest) = '"' :: loop rest
        loop (x::rest) = x :: loop rest
unescape _ = []

private
string : Grammar state TOMLToken True CValue
string = map (CVString . pack . unescape . unpack) $ match TTString

private
boolean : Grammar state TOMLToken True CValue
boolean = map CVBoolean $ match TTBoolean

private
integer : Grammar state TOMLToken True CValue
integer = map CVInteger $ match TTInt

private
float : Grammar state TOMLToken True CValue
float = map CVFloat $ match TTFloat

private
bare : Grammar state TOMLToken True String
bare = match TTBare

private
key : Grammar state TOMLToken True CKey
key = do
        first <- keyAtom
        rest <- many (punct Dot *> keyAtom)
        case rest of
            [] => pure $ CKAtom first
            rest => pure $ CKDotted (first:::rest)
    where
        keyAtom : Grammar state TOMLToken True CKeyAtom
        keyAtom = map CKBare bare
              <|> (map CKQuoted $ match TTString)

mutual
    private
    value : Grammar state TOMLToken True CValue
    value = string
        <|> boolean
        <|> integer
        <|> float
        <|> array
        <|> inlineTable
    
    private
    array : Grammar state TOMLToken True CValue
    array = do
        punct (Square Open)
        commit
        vals <- sepBy (allowNewlines $ punct Comma) (allowNewlines value)
        punct (Square Close)
        pure $ CVArray vals
    
    private
    inlineTable : Grammar state TOMLToken True CValue
    inlineTable = do
        punct (Curly Open)
        commit
        vals <- sepBy (punct Comma) $ do
            k <- key
            punct Equal
            v <- value
            pure (k, v)

        punct (Curly Close)
        pure $ CVInlineTable vals

private
keyValue : Grammar state TOMLToken True Item
keyValue = do
    k <- key
    punct Equal
    v <- value
    newlines
    pure $ IKeyValue k v

private
tableHeader : Grammar state TOMLToken True Item
tableHeader = do
    punct (Square Open)
    k <- key
    commit
    punct (Square Close)
    newlines
    pure $ ITableHeader k

private
tableArrayHeader : Grammar state TOMLToken True Item
tableArrayHeader = do
    punct (Square Open)
    punct (Square Open)
    commit
    k <- key
    punct (Square Close)
    punct (Square Close)
    newlines
    pure $ ITableArray k

private
item : Grammar state TOMLToken True Item
item = keyValue
   <|> tableHeader
   <|> tableArrayHeader

private
items : Grammar state TOMLToken False (List Item)
items = do
    maybeNewlines 
    is <- many item
    maybeNewlines
    eof
    pure is

export
parseItems : List (WithBounds TOMLToken) -> Either (List String) (List Item)
parseItems toks = case parse items $ filter (not . ignored) toks of
    Right (its, []) => Right its
    Right _ => Left ["unconsumed input"]
    Left errs => Left . flip map (forget errs) $ \(Error msg bounds) =>
        case bounds of
            Just bounds =>
                "\{show bounds.startLine}:\{show bounds.startCol}--\{show bounds.endLine}:\{show bounds.endCol}: \{msg}"
            Nothing => msg