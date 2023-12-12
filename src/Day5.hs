module Day5 (part1, part2) where

import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Maybe

part1 :: String -> IO Int
part1 input = return 69

part2 :: String -> IO Int
part2 input = return 420

-- Types

data Range = Range { from :: (Int,Int), to :: Int} deriving (Show)

type Layer = [Range]

inRange :: Range -> Int -> Bool
inRange (Range (from1, from2) _) n = n >= from1 && n <= from2

applyRange :: Range -> Int -> Int
applyRange (Range (from1, _) to) n = n - from1 + to

applyLayer :: Layer -> Int -> Int
applyLayer layer n =
    let mapped = mapMaybe (\range -> if inRange range n then Just $ applyRange range n else Nothing) layer
    in if null mapped then n else head mapped

applyLayers :: [Layer] -> Int -> Int
applyLayers layers n = foldl (flip applyLayer) n layers

-- Parsing

parseRange :: Parser Range
parseRange = do
    from <- decimal
    char ' '
    to <- decimal
    char ' '
    size <- decimal
    return $ Range (from, from + size - 1) to

parseLayer :: Parser Layer
parseLayer = do
    skipMany (letterChar <|> spaceChar <|> newline <|> char ':' <|> char '-')
    parseRange `sepBy` eol

parseLayers :: Parser [Layer]
parseLayers = parseLayer `sepBy` (newline >> newline) <* eof

parseInput :: Parser ([Int], [Layer])
parseInput = do
    string "seeds: "
    seeds <- decimal `sepBy` char ' ' <* newline
    layers <- parseLayers
    return (seeds, layers)