
-- cabal install split

import System.IO
import System.Environment
import System.FilePath
import Data.List.Split

inputFileName = "input_1.txt"

forward = "forward"
back = "back"
up = "up"
down = "down"

evaluate_instructions' :: (Int, Int) -> [(String, Int)] -> (Int, Int)
evaluate_instructions' state [] = state
evaluate_instructions' (x, y) ((dir, amount):xs)
    | dir == forward = evaluate_instructions' (x + amount, y) xs
    | dir == up = evaluate_instructions' (x, y - amount) xs
    | dir == down = evaluate_instructions' (x, y + amount) xs


evaluate_instructions :: [(String, Int)] -> (Int, Int)
evaluate_instructions instrlist = evaluate_instructions' (0, 0) instrlist


evaluate_instructions2' :: (Int, Int, Int) -> [(String, Int)] -> (Int, Int, Int)
evaluate_instructions2' state [] = state
evaluate_instructions2' (x, y, z) ((dir, amount):xs)
    | dir == forward = evaluate_instructions2' (x + amount, y + (z * amount), z) xs
    | dir == up = evaluate_instructions2' (x, y, z - amount) xs
    | dir == down = evaluate_instructions2' (x, y, z + amount) xs


evaluate_instructions2 :: [(String, Int)] -> (Int, Int, Int)
evaluate_instructions2 instrlist = evaluate_instructions2' (0, 0, 0) instrlist


main = do
    name <- getProgName
    args <- getArgs
    input <- readFile inputFileName
    let lines = filter (\x -> length x > 0) (splitOn "\n" input)
    let instructions = (map (\(x:y:[]) -> (x, read y)) $ map (splitOn " ") lines) :: [(String, Int)]

    let (x, y) = evaluate_instructions instructions
    putStrLn $ "Solution 1: " ++ show (x, y) ++ ": " ++ show (x * y)

    let (x1, y1, z1) = evaluate_instructions2 instructions
    putStrLn $ "Solution 2: " ++ show (x1, y1) ++ ": " ++ show (x1 * y1)
