module Day2.Main where
import Util.ArrayUtil
import Day2.Util

getPower :: [(Int, Int, Int)] -> Int
getPower [] = 0
getPower x =
    let minimums = foldl (\(a,b,c) (d,e,f) -> (max a d,max b e,max c f)) (0,0,0) x
        (r, g, b) = minimums
    in r * g * b

main :: IO ()
main = do
    input <- readFile "Day2/input.txt"
    let subsets = createSubsets input
    let games = mapGames subsets
    let power = sum $ map getPower games
    print power