module Day3 (run) where 

import Utils (toDec, rotateRight)
import Data.Bits (complement) 
import Data.List (intercalate)

run :: [String] -> (Int, Int)
run ls = (part1 ls', 0) where
	ls' = parseInput ls

part1 :: [String] -> Int
part1 b = accs where
	accs = solve [count (0, 0) b' | b' <- b]
	
solve :: [Acc] -> Int 
solve accs = g * e where
	g = toDec $ foldl gamma  "" accs
	e = toDec $ foldl epsilon "" accs

gamma :: String -> Acc -> String
gamma s (z, o) | z > o = "0" ++ s
			   | z < o = "1" ++ s

epsilon :: String -> Acc -> String
epsilon s (z, o) | z < o = "1" ++ s
			     | z > o = "0" ++ s

count :: Acc-> String -> Acc
count (az, ao) b = (z, o)  where
	z = length [c | c <- b, c == '0'] 
	o = length [c | c <- b, c == '1']

type Acc = (Int, Int)

parseInput :: [String] -> [String]
parseInput ls = rotateRight ls

