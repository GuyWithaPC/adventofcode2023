module Day2 (part1, part2) where
import Common.ArrayUtil

-- Main Methods

part1 :: String -> IO Int
part1 input =
    let subsets = createSubsets input
        games = zip [1..] (mapGames subsets)
        possibleGames = sum $ map fst $ filter (\x -> isPossible (snd x) gameBag) games
        in do return possibleGames

part2 :: String -> IO Int
part2 input =
    let subsets = createSubsets input
        games = mapGames subsets
        power = sum $ map getPower games
        in do return power

-- Utility

-- grabs and reverses a number based on a given index (coming from the right) in a string
grabNumberAtR :: Int -> String -> Int
grabNumberAtR _ [] = error "tried grabbing a number out of an empty string"
grabNumberAtR n x = read $ reverse $ takeWhile (/= ' ') $ drop' n $ reverse x

mapSubset :: String -> (Int, Int, Int)
mapSubset x =
    let indices = indicesR ["red", "green", "blue"] x
        red = if any (\x -> snd x == "red") indices then
            let index = fst (head $ filter (\x -> snd x == "red") indices) + length "red "
            in grabNumberAtR index x
        else 0
        green = if any (\x -> snd x == "green") indices then
            let index = fst (head $ filter (\x -> snd x == "green") indices) + length "green "
            in grabNumberAtR index x
        else 0
        blue = if any (\x -> snd x == "blue") indices then
            let index = fst (head $ filter (\x -> snd x == "blue") indices) + length "blue "
            in grabNumberAtR index x
        else 0
    in (red, green, blue)

mapGame :: [String] -> [(Int, Int, Int)]
mapGame [] = []
mapGame x = mapSubset (head x) : mapGame (tail x)

mapGames :: [[String]] -> [[(Int, Int, Int)]]
mapGames [] = []
mapGames x = mapGame (head x) : mapGames (tail x)

createSubsets :: String -> [[String]]
createSubsets [] = []
createSubsets x = map ((`collectGroups` ';') . drop' 2 . dropWhile (/= ':')) $ collectGroups x '\n'

-- Part 1

gameBag = (12, 13, 14)

isPossible :: [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
isPossible [] _ = True
isPossible x bag =
    let (r, g, b) = head x
        (br, bg, bb) = bag
        in not (r > br || g > bg || b > bb) && isPossible (tail x) bag

-- Part 2

getPower :: [(Int, Int, Int)] -> Int
getPower [] = 0
getPower x =
    let minimums = foldl (\(a,b,c) (d,e,f) -> (max a d,max b e,max c f)) (0,0,0) x
        (r, g, b) = minimums
    in r * g * b