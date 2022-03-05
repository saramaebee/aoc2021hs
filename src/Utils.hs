module Utils (run_day, readInputFile, parseIntsFromStrings) where

readInputFile :: FilePath -> IO [String]
readInputFile x = do
    contents <- readFile x
    pure $ lines contents


parseIntsFromStrings :: [String] -> [Int]
parseIntsFromStrings x = [readInt n | n <- x]


readInt :: String -> Int
readInt = read

run_day :: IO [String] -> ([String] -> (Int, Int)) -> IO () 
run_day ls f = do
    l <- ls;
    putStrLn (show $ f l)

