module Day1.Util where
import Data.Char (isDigit)

getFirstLast :: (Show a) => [a] -> [a]
getFirstLast x
    | null x = error "empty list"
    | length x == 1 = [head x, head x]
    | otherwise = [head x, last x]

getDigit :: String -> Int
getDigit x = read $ getFirstLast $ filter isDigit x