module Day2 where

import Utils (readInt, readInputFile)

run :: [String] -> (Int, Int)
run l = (part1 l, -1)

data Dir = Down | Up | Forward 
type ChPosReq = (Int, Dir)
type Pos = (Int, Int)
data Dir' = Depth | HorPoz
type ChPosReq' = (Int, Dir')

part1' :: Pos -> [String] -> Pos 
part1' p [] = p 
part1' p (c:cs) = let s = changePos p (parseRequest (parseCommand c)) in part1' s cs

part1 :: [String] -> Int 
part1 c = let t = part1' (0, 0) c in fst t * snd t

changePos :: Pos -> ChPosReq' -> Pos
changePos (d, h) (v, Depth) = (d+v, h)
changePos (d, h) (v, HorPoz) = (d, h+v) 

parseRequest :: ChPosReq -> ChPosReq'
parseRequest (n, Forward) = (n, HorPoz)
parseRequest (n, Down) = (n, Depth)
parseRequest (n, Up) = (-n, Depth)

parseCommand :: String -> ChPosReq
parseCommand s = let c = parseCommand' s in (snd c, parseDir (fst c))

parseCommand' :: String -> (String, Int)
parseCommand' s = let m = words s in (head m, readInt (head (tail m)))

parseDir :: String -> Dir
parseDir "forward" = Forward 
parseDir "down" = Down 
parseDir "up" = Up
