module Day7 (part1, part2) where
import qualified Data.Map as M
import Data.List
import Data.Function (on)
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

part1 :: String -> IO Int
part1 input = do
    hands <- case parse parseHands "" input of
        Left e -> error (errorBundlePretty e)
        Right r -> return r
    let sorted = sortBy (compareHands `on` fst) hands
        bids = map snd sorted
        winnings = sum $ zipWith (*) bids [1..]
    return winnings

part2 :: String -> IO Int
part2 input = do
    hands <- case parse parseHands' "" input of
        Left e -> error (errorBundlePretty e)
        Right r -> return r
    let sorted = sortBy (compareHands `on` fst) hands
        bids = map snd sorted
        winnings = sum $ zipWith (*) bids [1..]
    return winnings

-- Part 1

cards :: M.Map Char Int
cards = M.fromList [
    ('2', 1),
    ('3', 2),
    ('4', 3),
    ('5', 4),
    ('6', 5),
    ('7', 6),
    ('8', 7),
    ('9', 8),
    ('T', 9),
    ('J', 10),
    ('Q', 11),
    ('K', 12),
    ('A', 13)]

data Hand = Hand {hand :: [Int], strength :: Int} deriving (Show)

numDupes :: (Ord a) => [a] -> Int
numDupes = sum . filter (>1) . map length . group . sort

mergeNumbers :: [Int] -> [[Int]]
mergeNumbers l = sortBy (flip compare `on` length) (group (sort l))

isTwoPair :: [Int] -> Bool
isTwoPair hand = 
    let numbers = mergeNumbers hand
    in length numbers == 3 && (length (numbers !! 0) == 2) && (length (numbers !! 1) == 2)

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind hand = 
    let numbers = mergeNumbers hand
    in length numbers == 3 && (length (numbers !! 0) == 3)

isFullHouse :: [Int] -> Bool
isFullHouse hand = 
    let numbers = mergeNumbers hand
    in length numbers == 2 && (length (numbers !! 0) == 3)

getHandType :: [Int] -> Int
getHandType hand
    | numDupes hand == 0 = 1
    | numDupes hand == 2 = 2
    | isTwoPair hand = 3
    | isThreeOfAKind hand = 4
    | isFullHouse hand = 5
    | numDupes hand == 4 = 6
    | numDupes hand == 5 = 7
    | otherwise = 0

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2
    | strength h1 > strength h2 = GT
    | strength h1 < strength h2 = LT
    | otherwise = compare (hand h1) (hand h2)

-- Part 2

cards' :: M.Map Char Int
cards' = M.fromList [
    ('2', 1),
    ('3', 2),
    ('4', 3),
    ('5', 4),
    ('6', 5),
    ('7', 6),
    ('8', 7),
    ('9', 8),
    ('T', 9),
    ('J', 0),
    ('Q', 11),
    ('K', 12),
    ('A', 13)]

mergeNumbers' :: [Int] -> [[Int]]
mergeNumbers' l =
    let unmod = sortBy (flip compare `on` length) (group (sort l))
        jokers = case filter (\x -> head x == 0) unmod of
            [] -> []
            x -> head x
        others = filter (\x -> head x /= 0) unmod
        in if not $ null others then
                (jokers ++ head others) : tail others
            else
                unmod

getHandType' :: [Int] -> Int
getHandType' hand
    | numDupes' hand == 0 = 1
    | numDupes' hand == 2 = 2
    | isTwoPair' hand = 3
    | isThreeOfAKind' hand = 4
    | isFullHouse' hand = 5
    | numDupes' hand == 4 = 6
    | numDupes' hand == 5 = 7
    | otherwise = 0

numDupes' :: [Int] -> Int
numDupes' = sum . filter (>1) . map length . mergeNumbers'

isTwoPair' :: [Int] -> Bool
isTwoPair' hand = 
    let numbers = mergeNumbers' hand
    in length numbers == 3 && (length (numbers !! 0) == 2) && (length (numbers !! 1) == 2)

isThreeOfAKind' :: [Int] -> Bool
isThreeOfAKind' hand = 
    let numbers = mergeNumbers' hand
    in length numbers == 3 && (length (numbers !! 0) == 3)

isFullHouse' :: [Int] -> Bool
isFullHouse' hand = 
    let numbers = mergeNumbers' hand
    in length numbers == 2 && (length (numbers !! 0) == 3)

-- Parsers

parseHand :: Parser (Hand, Int)
parseHand = do
    cardLabels <- many alphaNumChar
    skipWhitespace
    bid <- decimal
    let cds = map (\c -> M.findWithDefault 0 c cards) cardLabels
        str = getHandType cds
    return (Hand cds str, bid)

parseHand' :: Parser (Hand, Int)
parseHand' = do
    cardLabels <- many alphaNumChar
    skipWhitespace
    bid <- decimal
    let cds = map (\c -> M.findWithDefault (-1) c cards') cardLabels
        str = getHandType' cds
    return (Hand cds str, bid)

parseHands :: Parser [(Hand, Int)]
parseHands = parseHand `sepBy` newline <* eof

parseHands' :: Parser [(Hand, Int)]
parseHands' = parseHand' `sepBy` newline <* eof