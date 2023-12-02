module Main where

import System.Environment

import qualified Day1 as D1 (part1, part2)
import qualified Day2 as D2 (part1, part2)

getDayInput :: Int -> IO String
getDayInput i = readFile $ "input/day" ++ show i ++ ".txt"

run :: (Show a) => Int -> [String -> a] -> IO ()
run day [part1, part2] = do
    input <- getDayInput day
    putStrLn $ "Day " ++ show day ++ ":"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> case day of
            "1" -> run 1 [D1.part1, D1.part2]
            "2" -> run 2 [D2.part1, D2.part2]
            _ -> putStrLn "Invalid day"
        _ -> putStrLn "Invalid arguments"
