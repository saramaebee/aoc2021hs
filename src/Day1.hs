module Day1 (run) where

import Utils (parseIntsFromStrings, windows,  readInputFile)

part1 :: [Int] -> Int
part1 x = fst $ foldl part1' (0, Nothing) x

-- increases, prev_depth
part1' :: (Int, Maybe Int) -> Int -> (Int, Maybe Int) 
part1' (acc, Nothing) newDepth = (acc, Just newDepth)
part1' (acc, Just prevDepth) newDepth = (if prevDepth < newDepth then succ acc else acc, Just newDepth)

part2 :: [Int] -> Int
part2 ls = fst $ foldl part1' (0, Nothing) $ map sum $ Utils.windows ls 3

run :: [String] -> (Int, Int)
run i' = (part1 i, part2 i) where i = parseIntsFromStrings i' 


