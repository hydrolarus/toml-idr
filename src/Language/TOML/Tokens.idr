-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Tokens

import Text.Bounded
import Text.Token

%default total

public export
strTrue : String
strTrue = "true"

public export
strFalse : String
strFalse = "false"

public export
data Bracket = Open | Close

export
Show Bracket where
    show Open = "open"
    show Close = "close"

public export
Eq Bracket where
    (==) Open Open = True
    (==) Close Close = True
    (==) _ _ = False

public export
data Punctuation
    = Comma
    | Dot
    | Equal
    | NewLine
    | Square Bracket
    | Curly Bracket

export
Show Punctuation where
    show Comma = ","
    show Dot = "."
    show Equal = "="
    show NewLine = "\\n"
    show (Square Open) = "["
    show (Square Close) = "]"
    show (Curly Open) = "("
    show (Curly Close) = ")"

public export
Eq Punctuation where
    (==) Comma Comma = True
    (==) Dot Dot = True
    (==) Equal Equal = True
    (==) NewLine NewLine = True
    (==) (Square x) (Square y) = x == y
    (==) (Curly x) (Curly y) = x == y
    (==) _ _ = False

||| What sort of string this is
||| Names are taken from the [toml spec](https://toml.io/en/v1.0.0#string)
public export
data StringType : Type where
    ||| Double quoted, single-line string
    ||| Supports escaping
    ||| "hello \n world"
    Basic : StringType
    ||| Single quoted, singe-line string
    ||| Does not support escaping
    ||| 'hello \n world'
    Literal : StringType
    -- TODO: the following
    ||| Double quoted, multi-line string
    ||| Supports escaping and line folding
    BasicMultiline : StringType
    ||| Single quoted, multi-line string
    ||| Does not support escaping or line folding
    LiteralMultiline : StringType

export
Show StringType where
    show Basic = "Basic"
    show Literal = "Literal"
    show BasicMultiline = "BasicMultiline"
    show LiteralMultiline = "LiteralMultiline"

export
Eq StringType where
    Basic == Basic = True
    Literal == Literal = True
    BasicMultiline == BasicMultiline = True
    LiteralMultiline == LiteralMultiline = True
    _ == _ = False


public export
data TOMLTokenKind
    = TTBoolean
    | TTInt
    | TTFloat
    | TTString StringType
    | TTPunct Punctuation
    | TTBare
    | TTIgnored

public export
TOMLToken : Type
TOMLToken = Token TOMLTokenKind

export
Show TOMLTokenKind where
    show TTBoolean = "boolean"
    show TTInt = "integer"
    show TTFloat = "float"
    show (TTString type) = "\{show type} string"
    show (TTPunct x) = show x
    show TTBare = "key"
    show TTIgnored = "comment"

public export
Eq TOMLTokenKind where
    (==) TTBoolean TTBoolean = True
    (==) TTInt TTInt = True
    (==) TTFloat TTFloat = True
    (==) (TTString x) (TTString y) = x == y
    (==) (TTPunct x) (TTPunct y) = x == y
    (==) TTBare TTBare = True
    (==) TTIgnored TTIgnored = True
    (==) _ _ = False

charToInt : Char -> Integer
charToInt c =
    if '0' <= c && c <= '9'
        then cast $ ord c - ord '0'
        else cast $ ord (toLower c) - ord 'a'

parameters (sign, base : Integer)
    private
    parseIntLoop :
        List Char ->
        (acc : Integer) ->
        Integer
    parseIntLoop [] acc = acc
    parseIntLoop ('_'::rest) acc = parseIntLoop rest acc
    parseIntLoop (c::rest) acc =
        parseIntLoop rest (acc * base + charToInt c * sign)

private
parseWithSign : List Char -> (sign : Integer) -> Integer
parseWithSign [] sign = 0
parseWithSign ('0'::'b'::rest) sign = parseIntLoop sign 2 rest 0
parseWithSign ('0'::'o'::rest) sign = parseIntLoop sign 8 rest 0
parseWithSign ('0'::'x'::rest) sign = parseIntLoop sign 16 rest 0
parseWithSign rest sign = parseIntLoop sign 10 rest 0

private
parseInt : List Char -> Integer
parseInt [] = 0
parseInt ('+'::rest) = parseWithSign rest 1
parseInt ('-'::rest) = parseWithSign rest (-1)
parseInt rest = parseWithSign rest 1

nan : Double
nan = sqrt (-1)

inf : Double
inf = 1.0 / 0.0

namespace Float
    parameters (sign : Double)
        ||| parse the exponent part of a double
        ||| not including the e
        parseExponent :
            (number : Double) ->
            (acc : Integer) ->
            List Char ->
            Double
        parseExponent number acc [] = sign * number * pow 10 (cast acc)
        parseExponent number acc ('_'::rest) = parseExponent number acc rest
        parseExponent number acc (x::rest) = parseExponent number (10 * acc + charToInt x) rest

        ||| parse the decimal part of a double
        ||| not including the .
        ||| up to and including the e
        parseDecimal :
            (whole : Integer) ->
            (acc : Double) ->
            (exponent : Double) ->
            List Char ->
            Double
        parseDecimal whole acc exponent [] = sign * (cast whole + acc)
        parseDecimal whole acc exponent ('e'::rest) = parseExponent (cast whole + acc) 0 rest
        parseDecimal whole acc exponent ('E'::rest) = parseExponent (cast whole + acc) 0 rest
        parseDecimal whole acc exponent ('_'::rest) = parseDecimal whole acc exponent rest
        parseDecimal whole acc exponent (x::rest) =
            parseDecimal
                whole
                (acc + cast (charToInt x) * exponent)
                (exponent * 0.1)
                rest

        ||| parse the part of the double before the `.`
        parseWhole : (acc : Integer) -> List Char -> Double
        parseWhole acc [] = 0.0
        parseWhole acc ('.'::rest) = parseDecimal acc 0.0 0.1 rest
        parseWhole acc ('e'::rest) = parseExponent (cast acc) 0 rest
        parseWhole acc ('E'::rest) = parseExponent (cast acc) 0 rest
        parseWhole acc ('_'::rest) = parseWhole acc rest
        parseWhole acc (x::rest) = parseWhole (10 * acc + charToInt x) rest

    parseSign : List Char -> Double
    parseSign ('+'::rest) = parseWhole 1.0 0 rest
    parseSign ('-'::rest) = parseWhole (-1.0) 0 rest
    parseSign rest = parseWhole 1.0 0 rest

    export
    parseFloat : String -> Double
    parseFloat "nan" = nan
    parseFloat "+nan" = nan
    parseFloat "-nan" = -nan
    parseFloat "inf" = inf
    parseFloat "+inf" = inf
    parseFloat "-inf" = -inf
    parseFloat x = parseSign (unpack x)

private
unescapeBasic : List Char -> Either String (List Char)
unescapeBasic ('"'::rest) = loop rest
    where
        -- hex digit to int
        hexToInt : Char -> Int
        hexToInt c =
            if '0' <= c && c <= '9'
                then ord c - ord '0'
                else ord (toLower c) - ord 'a'

        unicodeEscape : List Char -> Int

        loop : List Char -> Either String (List Char)
        loop [] = Left "unexpected end of input"
        loop ('"'::_) = Right []
        loop ('\\'::'b'::rest) = ('\b' ::) <$> loop rest
        loop ('\\'::'t'::rest) = ('\t' ::) <$> loop rest
        loop ('\\'::'n'::rest) = ('\n' ::) <$> loop rest
        loop ('\\'::'f'::rest) = ('\f' ::) <$> loop rest
        loop ('\\'::'r'::rest) = ('\r' ::) <$> loop rest
        loop ('\\'::'"'::rest) = ('"' ::) <$> loop rest
        loop ('\\'::'\\'::rest) = ('\\' ::) <$> loop rest
        loop ('\\'::'u'::u0::u1::u2::u3::rest) =
            (chr (unicodeEscape [u0, u1, u2, u3]) ::) <$> loop rest
        loop ('\\'::'U'::u0::u1::u2::u3::u4::u5::u6::u7::rest) =
            (chr (unicodeEscape [u0, u1, u2, u3, u4, u5, u6, u7]) ::) <$> loop rest
        loop ('\\'::_) = Left "invalid escape code"
        loop (x::rest) = (x ::) <$> loop rest
unescapeBasic _ = Left "expected quote"

public export
TokenKind TOMLTokenKind where
    TokType TTBoolean = Bool
    TokType TTInt = Integer
    TokType TTFloat = Double
    -- a string accepted by the lexer isn't necessarily
    -- a valid toml string
    TokType (TTString _) = Either String String
    TokType (TTPunct _) = ()
    TokType TTBare = String
    TokType TTIgnored = ()

    tokValue TTBoolean s = s == strTrue
    tokValue TTInt s = parseInt (unpack s)
    tokValue TTFloat s = parseFloat s

    tokValue (TTString Basic) s = map pack $ unescapeBasic (unpack s)
    tokValue (TTString Literal) s = Left "unimplemted string type: literal"
    tokValue (TTString BasicMultiline) s = Left "unimplemted string type: basic multiline"
    tokValue (TTString LiteralMultiline) s = Left "unimplemted string type: literal multiline"

    tokValue (TTPunct _) _ = ()
    tokValue TTBare s = s
    tokValue TTIgnored _ = ()

export
getString : Token TOMLTokenKind -> Maybe (Either String String)
getString tok@(Tok ((TTString _)) text) = Just (value tok)
getString _ = Nothing

export
getKeyString : Token TOMLTokenKind -> Maybe (Either String String)
getKeyString tok@(Tok ((TTString Basic)) text) = Just (value tok)
getKeyString tok@(Tok ((TTString Literal)) text) = Just (value tok)
getKeyString _ = Nothing

export
ignored : WithBounds TOMLToken -> Bool
ignored (MkBounded (Tok TTIgnored _) _ _) = True
ignored _ = False