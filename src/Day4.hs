module Day4 (run) where

import Utils (readInputFile, readInt, split, windows, rotateLeft, chunksOf, isNothing)
import GHC.Stack (HasCallStack)

run :: [String] -> (Int, Int)
run ls = (part1 g, -1)
    where 
        g = parseInput ls

-- Type Declarations {{{

type BingoValue = Int
type BingoBoard = [[Maybe BingoValue]]
data GameState  = GameState [BingoValue] [BingoBoard] (Maybe Int) Bool

-- }}}

--  Parsing  {{{ 

parseInput :: HasCallStack => [String] -> ([BingoValue], [BingoBoard]) 
parseInput (l:ls) = (values, boards)
  where 
    values = parseValues l
    boards = parseBoards ls

parseValues :: HasCallStack => String -> [BingoValue]
parseValues calls = map readInt $ split ',' calls

parseBoards :: HasCallStack => [String] -> [BingoBoard]
parseBoards ls = func <$> tail <$> chunksOf 6 ls
  where 
    func = (\x -> [[Just (readInt x')] | x' <- x])

--  }}}
        
-- Part 1 {{{

part1 :: HasCallStack => ([BingoValue], [BingoBoard]) -> Int
part1 (values, boards) = part1Val x 
  where 
    x = part1' (GameState values boards Nothing False)

part1' :: HasCallStack => GameState -> GameState
part1' (GameState [] x y z) = (GameState [] x y z)
part1' (GameState _ bs lc True) = (GameState [] bs lc True)
part1' (GameState (n:ns) boards _ _) = part1' (GameState ns bs (Just n) (hasWinningBoard bs))
  where
    bs = callNumberOnBoards n boards

part1Val :: HasCallStack => GameState -> Int
part1Val (GameState _ _ _ False)          = 0
part1Val (GameState _ bs (Just lastCalled) True) = sumOfUnmarked * lastCalled
  where
    sumOfUnmarked = 0

callNumberOnBoards :: HasCallStack => BingoValue -> [BingoBoard] -> [BingoBoard]
callNumberOnBoards n bs = callNumberOnBoard n <$> bs

callNumberOnBoard :: HasCallStack => BingoValue -> BingoBoard -> BingoBoard
-- takes bingval and bingoboard and returns a board with the value set to Nothing
callNumberOnBoard n bs = map (replaceJustWithNothing n) <$> bs

replaceJustWithNothing :: (HasCallStack, Eq a) => a -> Maybe a -> Maybe a
replaceJustWithNothing x y = case (y) of 
  Nothing -> Nothing 
  Just z -> if x == z then Nothing else Just z

-- }}}

hasWinningBoard :: [BingoBoard] -> Bool
hasWinningBoard bs = any isWinningBoard bs

isWinningBoard :: BingoBoard -> Bool
isWinningBoard board =  horizontal || vertical || diag || diag' -- diag bl->tr; diag' tl->br
  where
    horizontal = any (\b -> all isNothing b) board
    vertical   = any (\b -> all isNothing b) (rotateLeft board)
    diag       = False
    diag'      = False

