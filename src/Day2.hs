module Day2 where

import Utils (readInt, readInputFile)

run :: [String] -> (Int, Int)
run l = (part1 l', part2 l') where 
	l' = fmap parseCommand l

part1 :: [Command] -> Int
part1 c = d * hP where (d, hP, _) = foldl runCommand (0, 0, Nothing) c

part2 :: [Command] -> Int
part2 c = d * hP where (d, hP, a) = foldl runCommand' (0, 0, Just 0) c 

type Depth = Int
type HorizonalPosition = Int
type Aim = Int
data Direction = Forward | Up | Down 
type Length = Int
type Position = (Depth, HorizonalPosition, Maybe Aim)
type Command = (Direction, Length)

runCommand :: Position -> Command -> Position
runCommand (d, h, Nothing) (Forward, len) = (d, h + len, Nothing)
runCommand (d, h, Nothing) (Up, len) = (d - len, h, Nothing) 
runCommand (d, h, Nothing) (Down, len) = (d + len, h, Nothing)

runCommand' :: Position -> Command -> Position
runCommand' (d, h, Just a) (Forward, len) = (d + (a * len), h + len, Just a)
runCommand' (d, h, Just a) (Up, len) = (d, h, Just $ a - len) 
runCommand' (d, h, Just a) (Down, len) = (d, h, Just $ a + len)



parseCommand :: String -> Command
parseCommand s = (d, l) where
	(d':l':_) = words s
	d = parseDirection d' 
	l = parseLength l' 

parseDirection :: String -> Direction
parseDirection "forward" = Forward
parseDirection "up" = Up
parseDirection "down" = Down

parseLength :: String -> Length
parseLength s = read s
