module Day3 where

import Data.Char
import Data.List
import Control.Applicative

half xs = splitAt (length xs `div` 2) xs
cartProd = uncurry $ liftA2 (,)
priority x
  | isUpper x = ord x - 38
  | otherwise = ord x - 96
parse = priority . fst . head . nub . filter (uncurry (==)) . cartProd . half
p1 = sum . map parse . lines <$> readFile "d3"
