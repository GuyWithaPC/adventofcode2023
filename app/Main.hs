module Main where

import System.Environment
import qualified Data.Map as DM

import qualified Day1 as D1 (part1, part2)
import qualified Day2 as D2 (part1, part2)
import qualified Day3 as D3 (part1, part2)
import qualified Day4 as D4 (part1, part2)
import qualified Day5 as D5 (part1, part2)
import qualified Day6 as D6 (part1, part2)
import qualified Day7 as D7 (part1, part2)
import qualified Day8 as D8 (part1, part2)
import qualified Day9 as D9 (part1, part2)
import qualified Day10 as D10 (part1, part2)
import qualified Day11 as D11 (part1, part2)

type DayFunction = String -> IO Int

getDayInput :: Int -> IO String
getDayInput i = readFile $ "input/day" ++ show i ++ ".txt"

run :: Int -> [DayFunction] -> IO ()
run day [part1, part2] = do
    input <- getDayInput day
    p1 <- part1 input
    p2 <- part2 input
    putStrLn $ "Day " ++ show day ++ ":"
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> case day of
            "all" -> runAll 1
            _ -> case DM.lookup (read day) days of
                Just [part1, part2] -> run (read day) [part1, part2]
                Nothing -> putStrLn "Invalid day"
        _ -> putStrLn "Invalid arguments"

runAll :: Int -> IO ()
runAll x = do
    case DM.lookup x days of
        Just [part1, part2] -> do 
            run x [part1, part2]
            runAll (x + 1)
        Nothing -> return ()

days = DM.fromList [
    (1, [D1.part1, D1.part2]),
    (2, [D2.part1, D2.part2]),
    (3, [D3.part1, D3.part2]),
    (4, [D4.part1, D4.part2]),
    (5, [D5.part1, D5.part2]),
    (6, [D6.part1, D6.part2]),
    (7, [D7.part1, D7.part2]),
    (8, [D8.part1, D8.part2]),
    (9, [D9.part1, D9.part2]),
    (10, [D10.part1, D10.part2]),
    (11, [D11.part1, D11.part2])]