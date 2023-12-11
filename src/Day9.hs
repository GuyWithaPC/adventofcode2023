module Day9 (part1, part2) where
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

part1 :: String -> IO Int
part1 input = do
    let sequences = parseInput input
    let answer = sum $ map extrapolate sequences
    return answer

part2 :: String -> IO Int
part2 input = do
    let sequences = parseInput input
    print (allDerivatives $ head sequences)
    let answer = sum $ map extrapolateBack sequences
    return answer

-- Part 1

derive :: [Int] -> [Int]
derive [x] = []
derive (x:y:xs) = (y - x) : derive (y:xs)

allDerivatives :: [Int] -> [[Int]]
allDerivatives xs
    | all (==0) xs = [xs]
    | otherwise = xs : allDerivatives (derive xs)

integrate :: [[Int]] -> Int
integrate = sum . map last

extrapolate :: [Int] -> Int
extrapolate = integrate . allDerivatives

-- Part 2

integrateBack :: [[Int]] -> Int
integrateBack ders =
    let alternatingSigns = cycle [1, -1]
        first = head (head ders)
        rest = map head (tail ders)
        signedRest = zipWith (*) alternatingSigns rest
    in first - sum signedRest

extrapolateBack :: [Int] -> Int
extrapolateBack = integrateBack . allDerivatives

-- Parsers

parseInput :: String -> [[Int]]
parseInput s =
    let digits = map words (lines s)
    in map (map read) digits