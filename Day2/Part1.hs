module Day2.Main where
import Util.ArrayUtil
import Day2.Util

-- subsets will be grouped into tuples (r,g,b)

gameBag = (12, 13, 14)

-- pass starting values into it
isPossible :: [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
isPossible [] _ = True
isPossible x bag =
    let (r, g, b) = head x
        (br, bg, bb) = bag
        in not (r > br || g > bg || b > bb) && isPossible (tail x) bag

main :: IO ()
main = do
    input <- readFile "Day2/input.txt"
    let subsets = createSubsets input
    let games = zip [1..] (mapGames subsets)
    let possibleGames = sum $ map fst $ filter (\x -> isPossible (snd x) gameBag) games
    print possibleGames