module Day1 (run) where

import Utils (parseIntsFromStrings, readInputFile)

part1 :: [Int] -> Int
part1 x = fst $ foldl part1' (0, -1) x

-- increases, prev_depth
part1' :: (Int, Int) -> Int -> (Int, Int) 
part1' (acc, -1) newDepth = (acc, newDepth)
part1' (acc, prevDepth) newDepth = (acc + if prevDepth < newDepth then 1 else 0, newDepth)

part2 :: [Int] -> Int
part2 x = part2' 0 x

part2' :: Int -> [Int] -> Int
part2' a (l:ls) = if length ls > 1 then part2' (a + (if (l + sum (take 2 ls) >= sum (take 3 ls)) then 0 else 1)) ls else a


run :: [String] -> (Int, Int)
run i' = (let i = parseIntsFromStrings i' in (part1 i, part2 i)) 

