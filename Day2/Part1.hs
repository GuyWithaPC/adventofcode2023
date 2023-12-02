module Day2.Main where
import Util.ArrayUtil

createSubsets :: String -> [[String]]
createSubsets [] = []
createSubsets x = map ((`collectGroups` ';') . drop' 2 . dropWhile (/= ':')) $ collectGroups x '\n'

-- subsets will be grouped into tuples (r,g,b)

gameBag = (12, 13, 14)

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