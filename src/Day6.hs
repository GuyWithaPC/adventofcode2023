{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day6 (part1, part2) where
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

part1 :: String -> IO Int
part1 input = do
    rounds <- case parse parseInput "" input of
        Left e -> error $ errorBundlePretty e
        Right r -> return r
    let margins = map getMargin rounds
    let prod = product $ map (\(a, b) -> (b - a) + 1) margins
    return prod

part2 :: String -> IO Int
part2 input = do
    round <- case parse parseInput2 "" input of
        Left e -> error $ errorBundlePretty e
        Right r -> return r
    let margin = getMargin round
    return $ (snd margin - fst margin) + 1

-- Part 1

getMargin :: Round -> (Int, Int)
getMargin (time, distance) =
    let dt = fromIntegral time :: Double
        root = sqrt (fromIntegral (time ^ 2 - 4 * distance) :: Double)
        minMargin = (dt - root) / 2
        maxMargin = (dt + root) / 2
    in (ceiling minMargin, floor maxMargin)

-- Part 1 Parsers

type Round = (Int, Int)

parseInput :: Parser [Round]
parseInput = do
    times <- parseTimes
    distances <- parseDistances
    return $ zip times distances

parseTimes :: Parser [Int]
parseTimes = do
    string "Time:"
    skipWhitespace
    manyTill (do 
        time <- decimal
        skipWhitespace
        return time) eol

parseDistances :: Parser [Int]
parseDistances = do
    string "Distance:"
    skipWhitespace
    manyTill (do 
        distance <- decimal
        skipWhitespace
        return distance) eof

-- Part 2 Parsers

parseInput2 :: Parser Round
parseInput2 = do
    time <- parseTime2
    distance <- parseDistance2
    return (time, distance)

parseTime2 :: Parser Int
parseTime2 = do
    string "Time:"
    skipWhitespace
    digitChars <- manyTill (do
        digit <- digitChar
        skipWhitespace
        return digit) eol
    return $ read digitChars

parseDistance2 :: Parser Int
parseDistance2 = do
    string "Distance:"
    skipWhitespace
    digitChars <- manyTill (do
        digit <- digitChar
        skipWhitespace
        return digit) eof
    return $ read digitChars