module Day10 where

import Data.List

p1 = sum . zipWith (*) indices . traverse nth indices . scanl' (+) 1 . concatMap transmute . lines <$> readFile "d10"

indices = [20,60..220]

nth n xs = xs !! pred n

transmute "noop" = [0]
transmute ('a':'d':'d':'x':' ':rest) = [0, read rest]
