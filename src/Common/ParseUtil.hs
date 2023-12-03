module Common.ParseUtil where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

-- Types

type Parser = Parsec Void String

-- Consumers

skipCharacters :: [Char] -> Parser ()
skipCharacters = skipMany . choice . map char

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ choice [char ' ', char '\t']

skipWhitespaceN :: Parser ()
skipWhitespaceN = skipMany $ choice [char ' ', char '\t', newline]