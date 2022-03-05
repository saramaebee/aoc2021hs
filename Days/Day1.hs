module Day1 where

import Files


-- main = do 
  --  list <- parseInts $ readInputFile "d1.txt"
   -- pure $ part1 list

part1 :: [Int] -> Int 
part1 [] = 0
part1 x = part1' 0 x

-- last value, accumulator, list
part1' :: Int -> [Int] -> Int
part1' a (l:ls) = a + (if l >= (head ls) then 1 else 0)

run = do
    --list <- parseInts $ readInputFile "d1.txt";
    putStrLn "test"
    --putStrLn $ show $ part1 list
