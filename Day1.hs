module Day1 where

import Data.List
import Data.List.Split

parse = map (sum . map read) . splitWhen null . lines <$> readFile "d1"
p1 = maximum <$> parse
p2 = sum . take 3 . reverse . sort <$> parse
