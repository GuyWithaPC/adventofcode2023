module Day1.Part2 where
import Util.ArrayUtil
import Day1.Util

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

patternToDigit :: String -> String
patternToDigit x = snd $ head $ filter (\(a,b) -> a == x) patterns

patternIndicesL :: String -> [(Int, String)]
patternIndicesL x =
    let patIndices = indicesL (map fst patterns) x
    in map (\(a,b) -> (a, patternToDigit b)) patIndices
    

patternIndicesR :: String -> [(Int, String)]
patternIndicesR x = 
    let patIndices = indicesR (map fst patterns) x
    in map (\(a,b) -> (a, patternToDigit b)) patIndices

firstDigit :: String -> String
firstDigit x = snd $ head $ quicksort' (\(a,b) (c,d) -> compare a c) $ patternIndicesL x

lastDigit :: String -> String
lastDigit x = snd $ head $ quicksort' (\(a,b) (c,d) -> compare a c) $ patternIndicesR x

main :: IO ()
main = do
    input <- readFile "Day1/input.txt"
    let lines = collectGroups input '\n'
    let calibration = sum $ map (getDigit . (\x -> firstDigit x ++ lastDigit x)) lines
    print calibration