module Day11 (part1, part2) where
import Common.PyUtil

part1 :: String -> IO Int
part1 input = python "src/Python/Day11Part1.py"

part2 :: String -> IO Int
part2 input = python "src/Python/Day11Part2.py"