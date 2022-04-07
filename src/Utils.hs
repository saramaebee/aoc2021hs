module Utils (toDec, split, rotateRight, rotateLeft, windows, run_day, readInt, readInputFile, parseIntsFromStrings, chunksOf, isNothing) where

import Data.List
import Data.Char (digitToInt)

readInputFile :: FilePath -> IO [String]
readInputFile x = do
    contents <- readFile x
    pure $ lines contents


-- shamelessly stolen from https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parseIntsFromStrings :: [String] -> [Int]
parseIntsFromStrings x = [readInt n | n <- x]

split :: Char -> String -> [String]
split delim str = case break (== delim) str of
                (a, delim:b) -> a : split delim b
                (a, "")    -> [a]


isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

readInt :: String -> Int
readInt = read

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
  where 
    (ys, zs) = splitAt n xs

run_day :: IO [String] -> ([String] -> (Int, Int)) -> IO () 
run_day ls f = do
    l <- ls;
    putStrLn (show $ f l)

-- shamelessly stolen (but also read through and understood)
-- from https://github.com/jmleakakos/haskell-sliding-window/blob/master/SlidingWindow.hs
windows :: [a] -> Int -> [[a]]
windows list@(_:t) windowSize
	| length list == windowSize = (window:[])
	| otherwise = (window:rest)
	where 
		window = take windowSize list
		rest = windows t windowSize

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse
