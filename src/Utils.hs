module Utils (windows, run_day, readInt, readInputFile, parseIntsFromStrings) where

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

-- shamelessly stolen (but also read through and understood)
-- from https://github.com/jmleakakos/haskell-sliding-window/blob/master/SlidingWindow.hs
windows :: [a] -> Int -> [[a]]
windows list@(_:t) windowSize
	| length list == windowSize = (window:[])
	| otherwise = (window:rest)
	where 
		window = take windowSize list
		rest = windows t windowSize

