-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Lexer

import Text.Lexer
import Text.Token

import public Language.TOML.Tokens

%default total

-- TODO this is a valid JSON number, not a TOML float.
-- it's close enough but not correct.
private
floatLit : Lexer
floatLit
  = let sign  = is '-'
        whole = is '0' <|> range '1' '9' <+> many digit
        frac  = is '.' <+> digits
        exp   = like 'e' <+> opt (oneOf "+-") <+> digits in
        opt sign <+> whole <+> opt frac <+> opt exp

-- TODO doesn't handle prefix `+` yet or inline `_`
private
integerLit : Lexer
integerLit = intLit <|> hexLit

private
bareKey : Lexer
bareKey = some (alphaNum <|> is '_' <|> is '-')

-- TODO doesn't handle single quoted strings or escapes yet
private
stringLit : Lexer
stringLit = Text.Lexer.stringLit

private
tomlTokenMap : TokenMap TOMLToken
tomlTokenMap = toTokenMap $
    [
        (newline, TTPunct NewLine),
        (lineComment (is '#'), TTIgnored),
        (spaces, TTIgnored),
        (is ',', TTPunct Comma),
        (is '.', TTPunct Dot),
        (is '=', TTPunct Equal),
        (is '[', TTPunct $ Square Open),
        (is ']', TTPunct $ Square Close),
        (is '{', TTPunct $ Curly Open),
        (is '}', TTPunct $ Curly Close),
        (exact strTrue <|> exact strFalse, TTBoolean),
        (integerLit, TTInt),
        (floatLit, TTFloat),
        (Language.TOML.Lexer.stringLit, TTString), -- TODO doesn't handle all escapes
        (bareKey, TTBare)
    ]

export
lexTOML : String -> Maybe (List (WithBounds TOMLToken))
lexTOML str =
    case lex tomlTokenMap str of
        (tokens, (_, _, "")) => Just tokens
        _ => Nothing
