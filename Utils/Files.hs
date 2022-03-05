module Files where

import System.IO
import Control.Monad

readInputFile :: FilePath -> IO [String]
readInputFile x = do
    contents <- readFile x
    pure $ lines contents


parseInts :: IO [String] -> IO [Int]
parseInts xs = do
    x <- xs; 
    return [readInt n | n <- x]

readInt :: String -> Int
readInt = read

print :: String -> IO ()
print x = putStrLn x

