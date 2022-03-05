module Day1 (run) where

import Utils (parseIntsFromStrings, readInputFile)

part1 :: [Int] -> Int
part1 [] = 0
part1 x = part1' 0 x

part1' :: Int -> [Int] -> Int
part1' a [] = a
part1' a (l: ls) = if length ls > 0 then part1' (a + (if l >= (head ls) then 0 else 1)) ls else a

part2 :: [Int] -> Int
part2 x = part2' 0 x

part2' :: Int -> [Int] -> Int
part2' a (l:ls) = if length ls > 1 then part2' (a + (if (l + sum (take 2 ls) >= sum (take 3 ls)) then 0 else 1)) ls else a


run :: [String] -> (Int, Int)
run i' = (let i = parseIntsFromStrings i' in (part1 i, part2 i)) 
