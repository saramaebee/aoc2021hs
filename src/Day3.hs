module Day3 (run) where 

import Utils (toDec, rotateRight, rotateLeft)
import Data.Bits (complement) 
import Data.List (intercalate)

run :: [String] -> (Int, Int) 
run ls = (part1 ls', part2 ls) where
    ls' = parseInput ls

-- part 1
part1 :: [String] -> Int 
part1 b = accs where
    accs = solve [count (0, 0) b' | b' <- b]

    
solve :: [Acc] -> Int 
solve acc = g * e where (g, e) = solve' acc

solve' :: [Acc] -> (Int, Int) 
solve' accs = (g, e) where
    g = toDec $ foldl gamma  "" accs
    e = toDec $ foldl epsilon "" accs

gamma :: String -> Acc -> String
gamma s (z, o) | z > o = "0" ++ s
               | z < o = "1" ++ s

epsilon :: String -> Acc -> String
epsilon s (z, o) | z < o = "0" ++ s
                 | z > o = "1" ++ s

count :: Acc-> String -> Acc
count (az, ao) b = (z, o)  where
    z = length [c | c <- b, c == '0'] 
    o = length [c | c <- b, c == '1']

type Acc = (Int, Int)

parseInput :: [String] -> [String]
parseInput ls = rotateLeft ls

-- part 2

part2 :: [String] -> Int
part2 ls = c * o where
    o = oxygenRate ls 
    c = co2Rate ls 


oxygenRate :: [String] -> Int
oxygenRate input = toDec $ head $ mostCommonBit 0 input

co2Rate :: [String] -> Int 
co2Rate input = toDec $ head $ leastCommonBit 0 input

leastCommonBit :: Int -> [String] -> [String]
leastCommonBit pos list | length list == 1 = list
                         | otherwise = leastCommonBit (pos+1) [k | k <- list, k !! pos == lcb] where
    ls     = [ls' !! pos | ls' <- list]
    (z, o) = count (0, 0) ls
    lcb    = if o < z then '1' else '0'

mostCommonBit :: Int -> [String] -> [String] 
mostCommonBit pos list | length list == 1 = list
                       | otherwise = mostCommonBit (pos+1) [k | k<- list, k !! pos == mcb] where
    ls     = [ls' !! pos | ls' <- list]
    (z, o) = count (0, 0) ls 
    mcb    = if z > o then '0' else '1'
