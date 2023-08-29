module Day5 where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Extra

p1     = bimap crates inst . breakOn "\n\n" <$> readFile "d5test"
crates = filter (not . null) . map (filter isUpper) . splitOn "   "  . concat . transpose . lines . head . splitOn "\n 1"
inst :: String -> [[Integer]]
inst   = map (map read . filter (all isDigit) . words) . filter (not . null) . lines
