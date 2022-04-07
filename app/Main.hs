module Main where

import qualified Day1 (run)
import qualified Day2 (run)
import qualified Day3 (run)
import qualified Day4 (run)
import qualified Utils (run_day, readInputFile)

main :: IO ()
main = do
    Utils.run_day (Utils.readInputFile "real_data/d1.txt") Day1.run
    Utils.run_day (Utils.readInputFile "real_data/d2.txt") Day2.run
    Utils.run_day (Utils.readInputFile "real_data/d3.txt") Day3.run
    Utils.run_day (Utils.readInputFile "real_data/d4.txt") Day4.run
    putStrLn "Day 4 test:"
    Utils.run_day (Utils.readInputFile "test_data/d4.txt") Day4.run
