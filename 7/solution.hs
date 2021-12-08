
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


mean [] = error "Empty list has no mean."
mean list = div (sum list) (length list) 


distance :: [Int] -> Int -> [Int]
distance list point = map (\x -> abs $ x - point) list


constantCost :: Int -> Int
constantCost a = a


linearCost :: Int -> Int
-- linearCost a = div (a * (a + 1)) 2
linearCost a = sum [1..a]


distanceCost :: (Int -> Int) -> [Int] -> Int -> Int
distanceCost costFunction list point = sum $ map costFunction $ distance list point


minimizeDistance :: (Int -> Int) -> [Int] -> Int -> Int
minimizeDistance costFunction input currentValue
    | slideLeftDistance < currentDistance = minimizeDistance costFunction input slideLeftPoint
    | slideRightDistance < currentDistance = minimizeDistance costFunction input slideRightPoint
    | otherwise = currentValue
    where   slideLeftDistance = distanceCost costFunction input slideLeftPoint
            slideRightDistance = distanceCost costFunction input slideRightPoint
            currentDistance = distanceCost costFunction input currentValue
            slideLeftPoint = currentValue - 1
            slideRightPoint = currentValue + 1


runWithInput costFunction input = sum $ map (\x -> abs (x - p)) input where p = minimizeDistance costFunction input (mean input)


main = do
    name <- getProgName
    args <- getArgs
    exinput <- readFile exampleFileName
    input <- readFile inputFileName

    let excrabs = map read $ splitOn "," $ head $ filter nonEmpty $ splitOn "\n" exinput :: [Int]
    let crabs1 = map read $ splitOn "," $ head $ filter nonEmpty $ splitOn "\n" input :: [Int]

    -- mapM print rlines
    print excrabs

    putStrLn $ "Example: " ++ (show $ runWithInput constantCost excrabs)
    putStrLn $ "Solution 1: " ++ (show $ runWithInput constantCost crabs1)
    putStrLn $ "Solution 1: " ++ (show $ runWithInput linearCost crabs1)
