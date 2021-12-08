
-- cabal install split

import System.IO
import System.Environment
import System.FilePath
import Data.List.Split

inputFileName = "input_1.txt"


main = do
    name <- getProgName
    args <- getArgs
    input <- readFile inputFileName
    let lines = filter (\x -> length x > 0) (splitOn "\n" input)
    let typed = map (read::String->Int) lines

    -- Part 1
    putStrLn $ show $ length $ [(x, y) | (x, y) <- zip typed (tail typed), x < y]
    -- Part 2
    let smoothed = [sum [x, y, z] | (x, (y, z))<- zip typed $ zip (drop 1 typed) (drop 2 typed)]
    putStrLn $ show $ length $ [(x, y) | (x, y) <- zip smoothed (tail smoothed), x < y]
