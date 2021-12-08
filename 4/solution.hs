
-- cabal install split

import System.IO
import System.Environment
import System.FilePath
import Data.List.Split
import Text.Read

inputFileName = "input_1.txt"

data Field = Crossed Int | Value Int
    deriving Show
    
type Row = [Field]
type Game = [Row]

notEmpty :: [x] -> Bool
notEmpty [] = False
notEmpty _ = True

parseGames :: [String] -> [Game]
parseGames str = []


toGame :: String -> Game
toGame field = map (map Value) intGames
    where   games = filter notEmpty $ splitOn "\n" field
            nonEmptyGames = map (filter notEmpty . splitOn " ") games
            intGames = map (map read) nonEmptyGames


markRow :: Int -> Row -> Row
markRow _ [] = []

markRow checkValue ((Value x):xs)
    | x == checkValue = (Crossed x):(markRow checkValue xs)
    | otherwise = (Value x):(markRow checkValue xs)

markRow checkValue ((Crossed x):xs) = (Crossed x):(markRow checkValue xs)


markGame :: Int -> Game -> Game
markGame checkValue game = map (markRow checkValue) game


isCrossed :: Field -> Bool
isCrossed (Crossed _) = True
isCrossed _ = False

isValue :: Field -> Bool
isValue (Value _) = True
isValue _ = False

unpackField :: Field -> Int
unpackField (Value x) = x
unpackField (Crossed x) = x


isBingoRow :: Row -> Bool
isBingoRow [] = error "Empty row."
isBingoRow row = (foldl (&&) True $ map isCrossed row)


columnizeGame :: Game -> Game
columnizeGame [] = []
columnizeGame game
    | (foldl (+) 0 (map length game)) == 0 = []
    | otherwise = (map head game):(columnizeGame (map tail game))


isBingo :: Game -> Bool
isBingo [] = error "Empty game."
isBingo g = isRowBingo || isColumnBingo
    where   isRowBingo = foldl (||) False (map isBingoRow g)
            isColumnBingo = foldl (||) False (map isBingoRow (columnizeGame g))


stepGame :: Int -> Game -> Game
stepGame number game = map (markRow number) game

playGame :: [Int] -> Game -> Maybe (Game, Int)
playGame [] game = Nothing
playGame (number:xs) game
    | isBingo game = error "Game is already done."
    | isBingo nextGameState = Just (nextGameState, number)
    | otherwise = playGame xs nextGameState
    where nextGameState = stepGame number game


playMultiGame :: [Int] -> [Game] -> Maybe([Game], Int)
playMultiGame [] _ = Nothing
playMultiGame _ [] = Nothing
playMultiGame (x:xs) games
    | anyBingo = Just (bingos, x)
    | otherwise = playMultiGame xs nextGames
    where   nextGames = map (stepGame x) games
            bingos = filter isBingo nextGames
            anyBingo = length bingos > 0


evaluateAnswer1 :: Game -> Int -> Int
evaluateAnswer1 game rid = uncheckedSum * rid
    where   uncheckedSum = sum $ map unpackField $ filter isValue $ foldl (++) [] game


solution1 :: Maybe([Game], Int) -> String
solution1 Nothing = "Unsolvable."
solution1 (Just (games, value))
    | length games /= 1 = error ("Won games are too long / short." ++ (show $ length games))
    | otherwise = "Solution1: " ++ (show $ evaluateAnswer1 (head games) value)


main = do
    name <- getProgName
    args <- getArgs
    input <- readFile inputFileName
    let lines = (splitOn "\n\n" input)

    let drawnNumbers = (map read $ splitOn "," $ head lines) :: [Int]
    let games = map toGame $ drop 1 lines
    -- print games

    -- print $ markGame 4 [[Value 3, Value 4], [Value 3, Value 5]]

    -- print $ isBingo [[Value 1, Value 2, Crossed 3], [Crossed 1, Value 2, Crossed 3], [Crossed 1, Crossed 2, Crossed 3]]
    -- print $ isBingo [[Value 1, Value 2, Crossed 3], [Crossed 1, Value 2, Crossed 3], [Crossed 1, Crossed 2, Value 3]]
    -- print $ isBingo [[Value 1, Value 2, Crossed 3], [Crossed 1, Value 2, Crossed 3], [Value 1, Value 2, Crossed 3]]
    
    -- print $ columnizeGame [[Value x | x <- [1..3]], [Value x | x <- [4..6]], [Value x | x <- [7..9]]]
    -- print $ playGame [1, 4, 7] [[Value x | x <- [1..3]], [Value x | x <- [4..6]], [Value x | x <- [7..9]]]
    -- print $ playMultiGame drawnNumbers games

    let winningGames = playMultiGame drawnNumbers games

    putStrLn $ solution1 winningGames

    -- print games

    -- let drawnNumbers = map (read :: Int) $ splitOn "," $ head lines

    -- let games = parseGames $ drop 1 lines
    -- print drawnNumbers
    -- print games
    -- print games