-- SPDX-FileCopyrightText: 2021 The toml-idr developers
--
-- SPDX-License-Identifier: MPL-2.0

module Language.TOML.Lexer

import Text.Lexer
import Text.Token

import public Language.TOML.Tokens

%default total

private
nonZeroDigit : Lexer
nonZeroDigit = range '1' '9'

private
floatLit : Lexer
floatLit
  = let sign     = oneOf "+-"
        whole    = is '0' <|> nonZeroDigit <+> many (opt (is '_') <+> digit)
        frac     = the Lexer $ is '.' <+> digit <+> many (opt (is '_') <+> digit)
        exp      = the Lexer $ like 'e' <+> opt (oneOf "+-") <+> digits
        constant = exact "nan" <|> exact "inf" in
        opt sign <+> (
            whole <+> ((frac <+> opt exp) <|> exp)
            <|> constant)

private
sepIntLit : Lexer
sepIntLit = opt (oneOf "+-") <+> nonZeroDigit <+> many (is '_' <|> digit)

private
sepBaseLit : (pre : String) -> (digit : Lexer) -> Lexer
sepBaseLit pre digit =
    exact pre
    <+> digit
    <+> many (opt (is '_') <+> digit)

private
integerLit : Lexer
integerLit =
    (sepBaseLit "0x" hexDigit
    <|> sepBaseLit "0o" octDigit
    <|> sepBaseLit "0b" binDigit
    <|> sepIntLit) <+> reject (oneOf ".eE")

private
bareKey : Lexer
bareKey = some (alphaNum <|> is '_' <|> is '-')

-- TODO doesn't handle single quoted strings or escapes yet
private
basicStringLit : Lexer
basicStringLit = quote (is '"') (escape (is '\\') any <|> isNot '\\')

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
        -- TODO: other string types
        (Language.TOML.Lexer.basicStringLit, TTString Basic),
        (bareKey, TTBare)
    ]

export
lexTOML : String -> Maybe (List (WithBounds TOMLToken))
lexTOML str =
    case lex tomlTokenMap str of
        (tokens, (_, _, "")) => Just tokens
        _ => Nothing
