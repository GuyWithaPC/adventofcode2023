module Day1.Main where
import Util.ArrayUtil
import Day1.Util

main :: IO ()
main = do
    input <- readFile "Day1/input.txt"
    let lines = collectGroups input '\n'
    let calibration = sum $ map getDigit lines
    print calibration