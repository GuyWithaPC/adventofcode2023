module Util.ArrayUtil where

-- | This function removes the first element of a list
--   unless the list is empty in which case it remains unchanged.
--   It takes one argument, the list to remove the first element from.
removeFirst :: [a] -> [a]
removeFirst x
    | null x = []
    | otherwise = tail x

-- | This function collects a list into groups based on a separator.
--   It takes two arguments, the list to collect and the separator, respectively.
collectGroups :: (Eq a) => [a] -> a -> [[a]]
collectGroups [] _ = []
collectGroups x y =
    let upTo = takeWhile (/= y) x
        rest = removeFirst $ dropWhile (/= y) x -- removeFirst is removing the separator here
    in upTo : collectGroups rest y

groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n x
    | n > 0 = take n x : groupEvery n (drop n x)
    | otherwise = error "Can't call groupEvery with negative number"

indexOf :: (Eq a) => [a] -> [a] -> Int
indexOf [] _ = -1
indexOf _ [] = -1
indexOf x y
    | length x > length y = -1
    | take (length x) y == x = 0
    | otherwise = let index = indexOf x (tail y)
    in if index == -1 then -1 else index + 1

indicesL :: (Eq a) => [[a]] -> [a] -> [(Int, [a])]
indicesL patterns x = filter (\x -> fst x >= 0) $ map (\p -> (indexOf p x, p)) patterns

indicesR :: (Eq a) => [[a]] -> [a] -> [(Int, [a])]
indicesR patterns x = filter (\x -> fst x >= 0) $ map (\p -> (indexOf (reverse p) $ reverse x, p)) patterns

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort arr =
    let x:xs = arr
        smaller = quicksort [a | a <- xs, a <= x]
        bigger = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

quicksort' :: (a -> a -> Ordering) -> [a] -> [a]
quicksort' _ [] = []
quicksort' cmp arr = 
    let x:xs = arr
        smaller = quicksort' cmp [a | a <- xs, cmp a x /= GT]
        bigger = quicksort' cmp [a | a <- xs, cmp a x == GT]
    in smaller ++ [x] ++ bigger