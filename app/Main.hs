module Main where

import System.Environment
import qualified Data.Map as DM

import qualified Day1 as D1 (part1, part2)
import qualified Day2 as D2 (part1, part2)
import qualified Day3 as D3 (part1, part2)

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
    (3, [D3.part1, D3.part2])]