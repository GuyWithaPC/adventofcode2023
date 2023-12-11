module Day8 (part1, part2) where
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as M

part1 :: String -> IO Int
part1 input = do
    (path, nodes) <- case parse parseInput "" input of
        Left e -> error $ errorBundlePretty e
        Right x -> return x
    let cycPath = cycle path
    let nodeMap = M.fromList $ map (\n -> (name n, n)) nodes
    return $ traversePath 0 cycPath (nodeMap M.! "AAA") nodeMap

part2 :: String -> IO Int
part2 input = do
    (path, nodes) <- case parse parseInput "" input of
        Left e -> error $ errorBundlePretty e
        Right x -> return x
    let cycPath = cycle path
    let nodeMap = M.fromList $ map (\n -> (name n, n)) nodes
    let startNodes = filter (\n -> last (name n) == 'A') nodes
    let depths = map (\n -> traversePath' 0 cycPath n nodeMap) startNodes
    return $ combineTraversed depths

-- Part 1

traversePath :: Int -> Path -> Node -> M.Map String Node -> Int
traversePath depth path node@(Node left' right' name') nodeMap =
    let
        nextNode = case head path of
            L -> nodeMap M.! left'
            R -> nodeMap M.! right'
        nextDirections = tail path
    in if name nextNode == "ZZZ" then depth + 1 else traversePath (depth + 1) nextDirections nextNode nodeMap

-- Part 2

traversePath' :: Int -> Path -> Node -> M.Map String Node -> Int
traversePath' depth path node@(Node left' right' name') nodeMap =
    let
        nextNode = case head path of
            L -> nodeMap M.! left'
            R -> nodeMap M.! right'
        nextDirections = tail path
    in if last (name nextNode) == 'Z' then depth + 1 else traversePath' (depth + 1) nextDirections nextNode nodeMap

combineTraversed :: [Int] -> Int
combineTraversed= foldl (\acc x -> lcm acc x) 1

-- Types

data Node = Node { left :: String, right :: String, name :: String} deriving (Show, Eq)

data Dir = L | R deriving (Show, Eq)

type Path = [Dir]

-- Parsers

parsePath :: Parser Path
parsePath = many ((char 'L' >> return L) <|> (char 'R' >> return R)) <* eol

parseNode :: Parser Node
parseNode = do
    name <- many letterChar
    skipMany (choice [spaceChar, char '='])
    char '('
    left <- many letterChar
    string ", "
    right <- many letterChar
    char ')'
    return $ Node left right name

parseNodes :: Parser [Node]
parseNodes = parseNode `sepBy` newline <* eof

parseInput :: Parser (Path, [Node])
parseInput = do
    path <- parsePath
    newline
    nodes <- parseNodes
    return (path, nodes)