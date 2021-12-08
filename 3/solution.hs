
-- cabal install split

import System.IO
import System.Environment
import System.FilePath
import Data.List.Split

inputFileName = "input_1.txt"


countBits' :: [Int] -> (Int, Int) -> (Int, Int)
countBits' [] tup = tup
countBits' (x:[]) (ones, zeroes)
    | x == 1 = (ones + 1, zeroes)
    | x == 0 = (ones, zeroes + 1)

countBits' (x:xs) (ones, zeroes)
    | x == 1 = countBits' xs (ones + 1, zeroes)
    | x == 0 = countBits' xs (ones, zeroes + 1)


countBits :: [Int] -> (Int, Int)
countBits [] = (0, 0)
countBits list = countBits' list (0, 0)


mostCommonBit' :: [(Int, Int)] -> [Int] -> [Int]
mostCommonBit' ((ones, zeroes):[]) list
    | ones >= zeroes = (1:list)
    | ones < zeroes = (0:list)

mostCommonBit' ((ones, zeroes):xs) list
    | ones >= zeroes = mostCommonBit' xs (1:list)
    | ones < zeroes = mostCommonBit' xs (0:list)

mostCommonBit :: [(Int, Int)] -> [Int]
mostCommonBit list = mostCommonBit' list []


leastCommonBit' :: [(Int, Int)] -> [Int] -> [Int]
leastCommonBit' ((ones, zeroes):[]) list
    | ones < zeroes = (1:list)
    | ones >= zeroes = (0:list)

leastCommonBit' ((ones, zeroes):xs) list
    | ones < zeroes = leastCommonBit' xs (1:list)
    | ones >= zeroes = leastCommonBit' xs (0:list)

leastCommonBit :: [(Int, Int)] -> [Int]
leastCommonBit list = leastCommonBit' list []


filterBitsByMostCommon' :: [[Int]] -> [Int] -> Int -> [Int]
filterBitsByMostCommon' words mostCommonBist index = error "NYI"

filterBitsByMostCommon :: [[Int]] -> [Int] -> [Int]
filterBitsByMostCommon values mostCommonValue = filterBitsByMostCommon' values mostCommonValue 0


main = do
    name <- getProgName
    args <- getArgs
    input <- readFile inputFileName
    let lines = map (map read :: Int) $ filter (\x -> length x > 0) (splitOn "\n" input)
    let size = maximum $ map length lines

    let columnized = [[w !! i | w <- lines] | i <- [0..size - 1]]
    let bits = map countBits columnized
    let gamma = sum $ [x * (2 ^ i) | (x, i) <- zip (mostCommonBit bits) ([0..] :: [Int])]
    let epsilon = sum $ [x * (2 ^ i) | (x, i) <- zip (leastCommonBit bits) ([0..] :: [Int])]

    putStrLn $ "Solution 1: " ++ (show $ gamma * epsilon)

    putStrLn $ show $ mostCommonBit bits