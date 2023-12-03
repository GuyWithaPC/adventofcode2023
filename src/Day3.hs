module Day3 (part1, part2) where
import Common.ParseUtil
import Common.ArrayUtil
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Char
import Debug.Trace

-- Main Methods

part1 :: String -> Int
part1 input =
    let symbolLocations = getAllSymbolLocations input
    in sum $ concatMap (\l -> getPartNumbers 0 l symbolLocations) $ zip [0..] $ lines input


part2 :: String -> Int
part2 input = 0

-- Part 2 Helper Methods



-- Part 1 Helper Methods

-- | Returns the list of all part numbers in a given enumerated line from the start index onward,
--   provided the list of all symbol locations in the file.
getPartNumbers :: Int -> (Int,String) -> [[Bool]] -> [Int]
getPartNumbers start (lineNum, line) symbolLocations =
    let (end, num) = checkPartNumber start (lineNum, line) symbolLocations
        atEnd = start >= length line
    in 
    if atEnd then [] else
    case num of
        Just num -> num : getPartNumbers end (lineNum, line) symbolLocations
        Nothing -> getPartNumbers end (lineNum, line) symbolLocations

checkPartNumber :: Int -> (Int,String) -> [[Bool]] -> (Int, Maybe Int)
checkPartNumber start (lineNum, line) symbolLocations =
    if checkIsPartNumber start (lineNum, line) symbolLocations
        then let (num, end) = getInteger start line
            in trace ("got part number " ++ show num) (end, Just num)
        else (start + 1, Nothing)

checkIsPartNumber :: Int -> (Int,String) -> [[Bool]] -> Bool
checkIsPartNumber start (lineNum, line) symbolLocations =
    let currentlyDigit = isDigit (line !! start) in
    if currentlyDigit && searchAt (map (\(x,y) -> (x+start, y+lineNum)) searchPattern) symbolLocations
    then currentlyDigit
    else currentlyDigit && (start + 1) < length line && checkIsPartNumber (start + 1) (lineNum, line) symbolLocations

searchAt :: [(Int, Int)] -> [[Bool]] -> Bool
searchAt [] _ = False
searchAt ((x,y):xs) symbolLocations
    | y < 0 || y >= length symbolLocations || x < 0 || x >= length (symbolLocations !! y) =
        searchAt xs symbolLocations
    | otherwise = (symbolLocations !! y) !! x || searchAt xs symbolLocations

-- Parsers

getAllSymbolLocations :: String -> [[Bool]]
getAllSymbolLocations input = map getSymbolLocations $ lines input

getSymbolLocations :: String -> [Bool]
getSymbolLocations [] = []
getSymbolLocations (x:xs) = 
    case x of
        '.' -> False : getSymbolLocations xs
        c -> not (isDigit c) : getSymbolLocations xs

getInteger :: Int -> String -> (Int,Int)
getInteger start input =
    let droppedInput = drop' start input
        numDigits = countDigits droppedInput
        numString = take numDigits droppedInput
        num = read numString :: Int
        in (num, start + numDigits)

countDigits :: String -> Int
countDigits input = length $ takeWhile isDigit input

-- Constants
searchPattern :: [(Int,Int)]
searchPattern = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]