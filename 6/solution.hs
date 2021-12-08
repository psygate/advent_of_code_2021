
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
import Data.Maybe

inputFileName = "input_1.txt"
exampleFileName = "example.txt"



nonEmpty :: [a] -> Bool
nonEmpty [] = False
nonEmpty _ = True


type LanternFish = Int
type Generation = [Int]
type Population = Int
type PopulationListTuple = (Int, Population)


spawnLanternFishDelay = 7 - 1 -- -1 because 0 offset
newLanternFishSpawnLanternFishDelay = 7 + 2 - 1 -- -1 because 0 offset


resetLanternFish :: [LanternFish] -> [LanternFish]
resetLanternFish list = spawnLanternFishDelay:list


addNewLanternFish :: [LanternFish] -> [LanternFish]
addNewLanternFish list = newLanternFishSpawnLanternFishDelay:list


processDay' :: [LanternFish] -> [LanternFish] -> [LanternFish]
processDay' [] stack = stack
processDay' (x:xs) stack
    | x == 0 = processDay' xs (resetLanternFish $ addNewLanternFish stack)
    | otherwise = processDay' xs ((x - 1):stack)


processDay :: [LanternFish] -> [LanternFish]
processDay [] = []
processDay list = processDay' list []


simulate :: Int -> [LanternFish] -> [LanternFish]
simulate x list
    | x <= 0 = list
    | otherwise = simulate (x - 1) (processDay list)


insertIntoStack :: [PopulationListTuple] -> LanternFish -> [PopulationListTuple]
insertIntoStack [] f = [(f, 1)]
insertIntoStack ((a, b):xs) f
    | a == f = (a, b + 1):xs
    | otherwise = (a, b):(insertIntoStack xs f)

rawFishToDaysLeftList' :: [LanternFish] -> [PopulationListTuple] -> [PopulationListTuple]
rawFishToDaysLeftList' list [] = rawFishToDaysLeftList' list initPopList
rawFishToDaysLeftList' [] stack = stack
rawFishToDaysLeftList' list@(fish:xs) stack = rawFishToDaysLeftList' xs (insertIntoStack stack fish)

initPopList = [(x, 0) | x <- [0..8]]


rawFishToDaysLeftList :: [LanternFish] -> [PopulationListTuple]
rawFishToDaysLeftList [] = initPopList
rawFishToDaysLeftList list = rawFishToDaysLeftList' list initPopList

main = do
    name <- getProgName
    args <- getArgs
    exinput <- readFile exampleFileName
    input <- readFile inputFileName

    let exfish = map read $ splitOn "," $ head $ filter nonEmpty $ splitOn "\n" exinput :: [Int]
    let fish = map read $ splitOn "," $ head $ filter nonEmpty $ splitOn "\n" input :: [Int]

    -- mapM print rlines
    print exfish

    putStrLn $ "Example: " ++ (show $ length $ simulate 18 exfish)
    putStrLn $ "Example: " ++ (show $ length $ simulate 80 exfish)
    -- putStrLn $ "Solution 1: " ++ (show $ length $ simulate 80 fish)

    print fish
    print $ rawFishToDaysLeftList fish
    -- let fishByDaysLeft = rawFishToDaysLeftList fish

    -- putStrLn $ "Solution 2: " ++ (show slottedFish) -- (show $ length $ simulate 256 fish)

    -- let lines = filter isStraightLine $ map parseLineSegment (filter nonEmpty $ splitOn "\n" input)
    -- let points = sortBy comparePoints $ foldl (++) [] $ map toPoint lines
    -- let deduped = filter moreThanOneValue $ collectDuplicatePoints points

    -- putStrLn $ "Solution 1: " ++ (show $ length (map length deduped)) 
