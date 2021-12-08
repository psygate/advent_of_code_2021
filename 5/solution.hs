
-- cabal install split

import System.IO
import System.Environment
import System.FilePath
import Data.List.Split
import Data.List
import Text.Read
-- import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

inputFileName = "input_1.txt"
exampleFileName = "example.txt"

type Point = (Int, Int)
type Line =(Point, Point)


point x y = (x, y)


nonEmpty :: [a] -> Bool
nonEmpty x = length x > 0


strToPoint :: String -> Point
strToPoint str = (read $ head splitstr, read $ head $ drop 1 splitstr) where splitstr = splitOn "," str


comparePoints :: Point -> Point -> Ordering
comparePoints (a, b) (c, d)
    | a == c && b == d = EQ
    | a < c = LT
    | a > c = GT
    | a == c && b < d = LT
    | a == c && b > d = GT


dedupePoints' :: [Point] -> Point -> [Point] -> [[Point]] -> [[Point]]
dedupePoints' [] reference stack output = stack:output
dedupePoints' (x:xs) reference stack output
    | x == reference = dedupePoints' xs reference (x:stack) output
    | x /= reference = dedupePoints' (x:xs) x [] (stack:output)


dedupePoints :: [Point] -> [[Point]]
dedupePoints [] = []
dedupePoints list = dedupePoints' list (head list) [] []


toPoint :: String -> Point
toPoint pt = (lv, rv)
    where   base = splitOn "," pt
            lv = read (head base)
            rv = read (head $ drop 1 base)


toLine :: String -> Line
toLine str = (lp, rp) 
    where   base = splitOn " -> " str
            lp = toPoint (head base)
            rp = toPoint (head $ drop 1 base)


isStraightLine :: Line -> Bool
isStraightLine ((a, b), (c, d)) = (a == c && b /= d) || (a /= c && b == d)


lineToPoints :: Line -> [Point]
lineToPoints ((a, b), (c, d)) = [point x y | x <- [lxb..uxb], y <- [lyb..uyb]]
    where   lxb = min a c
            lyb = min b d
            uxb = max a c
            uyb = max b d



runWithInput name input = do
    let rlines = filter nonEmpty $ splitOn "\n" input
    let lines = map toLine rlines
    let straightLines = filter isStraightLine lines
    let pointSet = sortBy comparePoints $ foldl (++) [] $ map lineToPoints straightLines
    let dedupedPointSet = dedupePoints pointSet
    let duplicatePoints = filter (\x -> length x > 1) dedupedPointSet

    putStrLn $ name ++ ": " ++ (show $ length duplicatePoints)


runWithInput2 name input = do
    let rlines = filter nonEmpty $ splitOn "\n" input
    let lines = map toLine rlines
    let pointSet = sortBy comparePoints $ foldl (++) [] $ map lineToPoints lines
    let dedupedPointSet = dedupePoints pointSet
    let duplicatePoints = filter (\x -> length x > 1) dedupedPointSet

    putStrLn $ name ++ ": " ++ (show $ length duplicatePoints)


main = do
    name <- getProgName
    args <- getArgs
    input <- readFile inputFileName
    exampleInput <- readFile exampleFileName

    runWithInput "Example" exampleInput
    runWithInput "Solution 1" input
    runWithInput2 "Solution 2" input

    -- let lines = filter isStraightLine $ map parseLineSegment (filter nonEmpty $ splitOn "\n" input)
    -- let points = sortBy comparePoints $ foldl (++) [] $ map toPoint lines
    -- let deduped = filter moreThanOneValue $ collectDuplicatePoints points

    -- putStrLn $ "Solution 1: " ++ (show $ length (map length deduped)) 
