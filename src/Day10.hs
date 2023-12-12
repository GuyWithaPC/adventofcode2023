module Day10 (part1, part2) where
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace


part1 :: String -> IO Int
part1 input = do
    let pipes = parsePipes input
    let start = head $ filter (\p -> length (to p) == 4) pipes
    let others = filter (/= start) pipes
    let pipesMap = M.fromList $ map (\p -> (from p, p)) others
    let startPipe = head $ attached start pipesMap
    print start
    return $ (runLoop startPipe [] pipesMap 0 `div` 2) + 1

part2 :: String -> IO Int
part2 input = return 420

-- Part 1

attached :: Pipe -> M.Map (Int, Int) Pipe -> [Pipe]
attached pipe pipes = mapMaybe (`M.lookup` pipes) $ to pipe

runLoop :: Pipe -> [Pipe] -> M.Map (Int, Int) Pipe -> Int -> Int
runLoop pipe visited pipes count = case attached pipe pipes of
    [] -> count
    [p] -> if p `elem` visited then count else runLoop p (pipe : visited) pipes (count + 1)
    [p1, p2] -> if p1 `elem` visited then runLoop p2 (pipe : visited) pipes (count + 1) else runLoop p1 (pipe : visited) pipes (count + 1)

-- Types

data Pipe = Pipe { from :: (Int, Int), to :: [(Int, Int)]} deriving (Show)

instance Eq Pipe where
    (==) p1 p2 = from p1 == from p2

-- Parsing

parsePipes :: String -> [Pipe]
parsePipes input = concat $ zipWith (curry parsePipeLine) [0..] $ lines input

parsePipeLine :: (Int, String) -> [Pipe]
parsePipeLine (y, line) = mapMaybe (\(x,c) -> parsePipe x y c) $ zip [0..] line

parsePipe :: Int -> Int -> Char -> Maybe Pipe
parsePipe x y '.' = Nothing
parsePipe x y 'S' = Just $ Pipe (x, y) [(x,y+1), (x,y-1), (x+1,y), (x-1,y)] -- Start
parsePipe x y '|' = Just $ Pipe (x, y) [(x, y+1), (x, y-1)] -- Vertical
parsePipe x y '-' = Just $ Pipe (x, y) [(x+1, y), (x-1, y)] -- Horizontal
parsePipe x y 'L' = Just $ Pipe (x, y) [(x, y-1), (x+1, y)] -- North + East
parsePipe x y 'J' = Just $ Pipe (x, y) [(x, y-1), (x-1, y)] -- North + West
parsePipe x y '7' = Just $ Pipe (x, y) [(x, y+1), (x-1, y)] -- South + West
parsePipe x y 'F' = Just $ Pipe (x, y) [(x, y+1), (x+1, y)] -- South + East