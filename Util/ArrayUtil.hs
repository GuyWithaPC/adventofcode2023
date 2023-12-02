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

-- | This function collects a list into groups with a given size.
--   It takes two arguments, the group size and the list, respectively.
--   If the list can't be grouped evenly into groups of the given size, the last group will be smaller than the given size.
groupEvery :: Int -> [a] -> [[a]]
groupEvery _ [] = []
groupEvery n x
    | n > 0 = take n x : groupEvery n (drop n x)
    | otherwise = error "Can't call groupEvery with negative number"

-- | This function finds the start index of a pattern in a list.
--   It takes two arguments, the pattern and the array, respectively.
--   If the pattern does not exist in the list, the function will return -1.
indexOf :: (Eq a) => [a] -> [a] -> Int
indexOf [] _ = -1
indexOf _ [] = -1
indexOf x y
    | length x > length y = -1
    | take (length x) y == x = 0
    | otherwise = let index = indexOf x (tail y)
    in if index == -1 then -1 else index + 1

-- | This function finds every index at which a pattern occurs in a list.
--   It takes two arguments, the pattern and the array, respectively.
--   If the pattern does not exist in the list, the function will return an empty list.
indexesOf :: (Eq a) => [a] -> [a] -> [Int]
indexesOf [] _ = []
indexesOf _ [] = []
indexesOf x y
    | indexOf x y == -1 = []
    | otherwise = indexOf x y : indexesOf x (drop (indexOf x y + 1) y)

-- | This function replaces the first instance of a pattern in a list with another pattern.
--   It takes three arguments, the pattern to replace, the pattern to replace it with, and the list to replace it in, respectively.
--   If the pattern to replace does not exist in the list, the function will return the list unchanged.
replaceFirst :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst x y z
    | indexOf x z == -1 = z
    | otherwise = take (indexOf x z) z ++ y ++ drop (indexOf x z + length x) z

-- | This function replaces every instance of a pattern in a list with another pattern.
--   It takes three arguments, the pattern to replace, the pattern to replace it with, and the list to replace it in, respectively.
--   If the pattern to replace does not exist in the list, the function will return the list unchanged.
replaceAll :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceAll _ _ [] = []
replaceAll x y z
    | indexOf x z == -1 = z
    | otherwise = replaceAll x y $ replaceFirst x y z

-- | This function finds the earliest index of each pattern for a list of given patterns.
--   It takes two arguments, the list of patterns and the list to search through, respectively.
--   It returns the patterns and their indices as tuples in a list, and if a pattern does not exist it has an index of -1.
indicesL :: (Eq a) => [[a]] -> [a] -> [(Int, [a])]
indicesL patterns x = filter (\x -> fst x >= 0) $ map (\p -> (indexOf p x, p)) patterns

-- | This function is essentially an alias for indicesR, except that it searches right to left.
indicesR :: (Eq a) => [[a]] -> [a] -> [(Int, [a])]
indicesR patterns x = filter (\x -> fst x >= 0) $ map (\p -> (indexOf (reverse p) $ reverse x, p)) patterns

-- | This function takes an array of orderable elements and sorts it smallest to largest.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort arr =
    let x:xs = arr
        smaller = quicksort [a | a <- xs, a <= x]
        bigger = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

-- | This function takes an array of any elements and a function to order them, and sorts them smallest to largest.
quicksort' :: (a -> a -> Ordering) -> [a] -> [a]
quicksort' _ [] = []
quicksort' cmp arr = 
    let x:xs = arr
        smaller = quicksort' cmp [a | a <- xs, cmp a x /= GT]
        bigger = quicksort' cmp [a | a <- xs, cmp a x == GT]
    in smaller ++ [x] ++ bigger
