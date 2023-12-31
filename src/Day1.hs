module Day1 (part1, part2) where
import Common.ArrayUtil
import Data.Char
import Data.Bifunctor (second)


-- Main Methods

part1 :: String -> IO Int
part1 input =
    let lines = collectGroups input '\n'
        calibration = sum $ map getDigit lines
        in do return calibration

part2 :: String -> IO Int
part2 input =
    let lines = collectGroups input '\n'
        calibration = sum $ map (getDigit . (\x -> firstDigit' x ++ lastDigit' x)) lines
        in do return calibration

-- Part 1

getFirstLast :: (Show a) => [a] -> [a]
getFirstLast x
    | null x = error "empty list"
    | length x == 1 = [head x, head x]
    | otherwise = [head x, last x]

getDigit :: String -> Int
getDigit x = read $ getFirstLast $ filter isDigit x

-- Part 2

patternToDigit :: String -> String
patternToDigit x = snd $ head $ filter (\(a,b) -> a == x) patterns

patternIndicesL :: String -> [(Int, String)]
patternIndicesL x =
    let patIndices = indicesL (map fst patterns) x
    in map (second patternToDigit) patIndices
    

patternIndicesR :: String -> [(Int, String)]
patternIndicesR x = 
    let patIndices = indicesR (map fst patterns) x
    in map (second patternToDigit) patIndices

firstDigit :: String -> String
firstDigit x = snd $ head $ quicksort' (\(a,b) (c,d) -> compare a c) $ patternIndicesL x

-- | added after the fact, this version of firstDigit doesn't use quicksort (made the code take forever)
firstDigit' :: String -> String
firstDigit' x = snd $ foldl (\(a,b) (c,d) -> if a < c then (a,b) else (c,d)) (1000, "") $ patternIndicesL x

lastDigit :: String -> String
lastDigit x = snd $ head $ quicksort' (\(a,b) (c,d) -> compare a c) $ patternIndicesR x

-- | same as firstDigit'
lastDigit' :: String -> String
lastDigit' x = snd $ foldl (\(a,b) (c,d) -> if a < c then (a,b) else (c,d)) (1000, "") $ patternIndicesR x

-- Data

patterns = [
    ("0", "0"), ("zero", "0"),
    ("1", "1"), ("one", "1"),
    ("2", "2"), ("two", "2"),
    ("3", "3"), ("three", "3"),
    ("4", "4"), ("four", "4"),
    ("5", "5"), ("five", "5"),
    ("6", "6"), ("six", "6"),
    ("7", "7"), ("seven", "7"),
    ("8", "8"), ("eight", "8"),
    ("9", "9"), ("nine", "9")]