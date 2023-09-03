module Day8 where

import Control.Monad
import Data.Char
import Data.List
import Data.Tuple.Extra

p1 = trues . transOr . both visible . transPair . digits . lines <$> readFile "d8" where
  digits    = map $ map digitToInt
  transPair = id &&& transpose
  visible   = map $ liftM2 zipOr (vis mapAccumL) (vis mapAccumR)
  transOr   = concatMap (uncurry zipOr) . uncurry zip . second transpose
  trues     = length . filter id
  zipOr     = zipWith (||)
  vis f     = snd . f (\max x -> if (x > max) then (x, True) else (max, False)) (-1)
