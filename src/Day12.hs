module Day12 (part1, part2) where
import Common.ParseUtil
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (void)
import Debug.Trace

part1 :: String -> IO Int
part1 input = do
    lines <- case parse parseInput "" input of
        Left e -> error $ errorBundlePretty e
        Right l -> return l
    let possibilities = map (uncurry numPossibilities) lines
    return $ sum possibilities

part2 :: String -> IO Int
part2 input = do
    lines <- case parse parseInput "" input of
        Left e -> error $ errorBundlePretty e
        Right l -> return l
    let expandedLines = map expandLine lines
        possibilities = map (\(line, x) -> trace ("working on line " ++ show line) uncurry numPossibilities x) (zip [1..] expandedLines)
    return $ sum possibilities

-- Part 2

expandLine :: ([SpringState], [Int]) -> ([SpringState], [Int])
expandLine (states, rules) =
    let states' = init $ concat $ replicate 5 (states ++ [Unknown])
        rules' = concat $ replicate 5 rules
    in (states', rules')

-- Part 1

-- | return a list of the starts and lengths of ranges of the same SpringState
getRanges :: [SpringState] -> [(SpringState, Int, Int)]
getRanges [] = []
getRanges (s:ss) = ranges' s 0 0 ss
    where
        ranges' :: SpringState -> Int -> Int -> [SpringState] -> [(SpringState, Int, Int)]
        ranges' s st ed [] = [(s, st, ed - st + 1)]
        ranges' s st ed (s':ss) = if s == s' then ranges' s st (ed+1) ss else (s, st, ed - st + 1) : ranges' s' (ed+1) (ed+1) ss

numPossibilities :: [SpringState] -> [Int] -> Int
numPossibilities states rules = case validate states rules of
    Valid -> 1
    Invalid -> 0
    UnknownValidity ->
        let brokenChoice = chooseFirst states Broken
            operationalChoice = chooseFirst states Operational
            -- for debugging
            brokeInvalid = validate brokenChoice rules == Invalid
            operationalInvalid = validate operationalChoice rules == Invalid
        in {- trace ("<BRANCH>\nS: " ++ show states ++ "\n" ++ "B: " ++ show brokenChoice ++ " " ++ show brokeInvalid ++ "\n" ++ "O: " ++ show operationalChoice ++ " " ++ show operationalInvalid) -}
        numPossibilities brokenChoice rules + numPossibilities operationalChoice rules

chooseFirst :: [SpringState] -> SpringState -> [SpringState]
chooseFirst (s:ss) choose
    | s == Unknown = choose : ss
    | otherwise = s : chooseFirst ss choose

-- for all intents, Operational is the same as None
lookForward :: [SpringState] -> Int -> (SpringState, Int)
lookForward states i =
        let ranges = getRanges states
            hits = dropWhile (\(_, st, ln) -> st+ln < i+1) ranges
        in if null hits then (Operational, 0) else
            let (state, _, length) = head hits
            in (state, length)

lookBackward :: [SpringState] -> Int -> (SpringState, Int)
lookBackward states i =
        let ranges = getRanges states
            hits = takeWhile (\(_, st, _) -> st < i) ranges
        in if null hits then (Operational, 0) else
            let (state, _, length) = last hits
            in (state, length)

anyBefore :: [SpringState] -> SpringState -> Int -> Bool
anyBefore states state i =
    let ranges = getRanges states
        hits = filter (\(s, st, ln) -> s == state && st < i) ranges
    in not $ null hits

validate :: [SpringState] -> [Int] -> Validity
validate states = validate' (getRanges states)
    where
        validate' :: [(SpringState, Int, Int)] -> [Int] -> Validity
        validate' [] [] = Valid
        validate' [] _ = if Unknown `elem` states then UnknownValidity else Invalid
        validate' ranges [] = case ranges of
            (Broken, _, _):_ -> Invalid
            (x:xs) -> validate' xs []
        validate' ((state, pos, length):ranges) (rule:rules)
            | state == Broken && length == rule = validate' ranges rules
            | state == Broken && length < rule = case (lookForward states (pos+length), lookBackward states pos) of
                ((Broken, _), _) -> error "broken spring ranges next to each other"
                (_, (Broken, _)) -> error "broken spring ranges next to each other"
                ((Operational, _), (Operational, _)) -> if anyBefore states Unknown pos then UnknownValidity else Invalid
                _ -> UnknownValidity
            | state == Broken && length > rule = if anyBefore states Unknown pos then UnknownValidity else Invalid
            | otherwise = validate' ranges (rule:rules)

-- Types

data SpringState = Broken | Operational | Unknown deriving (Eq)
data Validity = Valid | Invalid | UnknownValidity deriving (Show, Eq)

-- Parsing

parseInput :: Parser [([SpringState], [Int])]
parseInput = many (do
    state <- parseStates
    rules <- parseRules
    return (state, rules)) <* eof

parseStates :: Parser [SpringState]
parseStates = do
    many parseState <* space1

parseState :: Parser SpringState
parseState = choice
    [ Broken <$ char '#',
      Operational <$ char '.',
      Unknown <$ char '?' ]

parseRules :: Parser [Int]
parseRules = decimal `sepBy` char ',' <* (void eol <|> eof)

-- Debugging

instance Show SpringState where
    show Broken = "#"
    show Operational = "."
    show Unknown = "?"

instance {-# OVERLAPPING #-} Show [SpringState] where
    show [] = ""
    show (s:ss) = show s ++ show ss