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

public export
Eq Punctuation where
    (==) Comma Comma = True
    (==) Dot Dot = True
    (==) Equal Equal = True
    (==) NewLine NewLine = True
    (==) (Square x) (Square y) = x == y
    (==) (Curly x) (Curly y) = x == y
    (==) _ _ = False

public export
data TOMLTokenKind
    = TTBoolean
    | TTInt
    | TTFloat
    | TTString
    | TTPunct Punctuation
    | TTBare
    | TTIgnored

public export
TOMLToken : Type
TOMLToken = Token TOMLTokenKind

public export
Eq TOMLTokenKind where
    (==) TTBoolean TTBoolean = True
    (==) TTInt TTInt = True
    (==) TTFloat TTFloat = True
    (==) TTString TTString = True
    (==) (TTPunct x) (TTPunct y) = x == y
    (==) TTBare TTBare = True
    (==) TTIgnored TTIgnored = True
    (==) _ _ = False


public export
TokenKind TOMLTokenKind where
    TokType TTBoolean = Bool
    TokType TTInt = Integer
    TokType TTFloat = Double
    TokType TTString = String
    TokType (TTPunct _) = ()
    TokType TTBare = String
    TokType TTIgnored = ()

    tokValue TTBoolean s = s == strTrue
    tokValue TTInt s = cast s
    tokValue TTFloat s = cast s
    tokValue TTString s = s -- TODO "unescape" the string
    tokValue (TTPunct _) _ = ()
    tokValue TTBare s = s
    tokValue TTIgnored _ = ()


export
ignored : WithBounds TOMLToken -> Bool
ignored (MkBounded (Tok TTIgnored _) _ _) = True
ignored _ = False