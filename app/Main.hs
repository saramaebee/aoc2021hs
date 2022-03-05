module Main where

import qualified Day1 (run)
import qualified Utils (run_day, readInputFile)

main :: IO ()
main = do
    Utils.run_day (Utils.readInputFile "real_data/d1.txt") Day1.run

