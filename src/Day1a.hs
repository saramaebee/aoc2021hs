module Day1a (run) where

import Utils (parseIntsFromStrings)

run :: [String] -> (Int, Int)
run i = (part1 i', part2 i') where
	i' = Utils.parseIntsFromStrings i 

part1 :: [Int] -> Int
part1 ls = foldr (\([fs, sn]:_) -> if fs < sn then 1 else 0 ) 0 ls

part2:: [Int] -> Int
part2 = -1

test :: Int
test = 1
