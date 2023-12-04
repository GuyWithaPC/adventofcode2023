{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Day4 (part1, part2) where
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Printf

import qualified Data.Map as M

import Debug.Trace

part1 :: String -> IO Int
part1 input = do
    let cards = map (\s -> case parse parseCard "" s of
                            Right x -> x
                            Left x -> error $ errorBundlePretty x) $ lines input
    let value = sum $ map getValue cards
    return value

part2 :: String -> IO Int
part2 input = do
    let cards = map (\s -> case parse parseCard "" s of
                            Right x -> x
                            Left x -> error $ errorBundlePretty x) $ lines input
    let cardMap = M.fromList $ map (\card -> (num card, card)) cards
    let countMap = M.fromList $ map (\card -> (num card, 1)) cards
    let initialCount = length cards
    let count = countCards cardMap countMap
    return $ initialCount + count

-- Day 1 Helpers

getValue :: Card -> Int
getValue card = 
    let wins = winCount card
    in if wins == 0 then 0 else 2 ^ (wins - 1)

-- Day 2 Helpers

winCount :: Card -> Int
winCount card = length $ filter (`elem` want card) (have card)

addedCards :: Card -> [Int]
addedCards card = [(num card + 1)..(num card + winCount card)]

countCards :: M.Map Int Card -> M.Map Int Int -> Int
countCards cardMap countMap
    | M.size countMap == 0 = 0
    | otherwise =
        let (cardNum, count) = M.findMin countMap
            card = cardMap M.! cardNum
            newCountMap = M.delete cardNum countMap
            newCards = addedCards card
            newCountMap' = foldl (\m c -> M.insertWith (+) c count m) newCountMap newCards
            in (length newCards * count) + countCards cardMap newCountMap'


-- Types

data Card = Card {
    num :: Int,
    want :: [Int],
    have :: [Int]
} deriving (Show)

-- Parsers

parseCard :: Parser Card
parseCard = do
    num <- parseNum
    want <- parseWant
    have <- parseHave
    return $ Card num want have

parseNum :: Parser Int
parseNum = do
    string "Card"
    skipWhitespace
    decimal

parseWant :: Parser [Int]
parseWant = do
    char ':'
    skipWhitespace
    manyTill (do 
                num <- decimal
                skipWhitespace
                return num) (char '|')

parseHave :: Parser [Int]
parseHave = do
    skipWhitespace
    manyTill (do 
        num <- decimal
        skipWhitespace
        return num) eof
