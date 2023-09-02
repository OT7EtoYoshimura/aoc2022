module Day8 where

import Control.Monad
import Data.Char
import Data.List
import Data.Tuple.Extra

p1 = trues . transOr . uncurry zip . second transpose . both (revOr . revPair) . transPair . digits . lines <$> readFile "d8" where
  digits = map $ map digitToInt
  transPair = toSnd transpose
  revPair = map $ toSnd reverse
  revOr = map $ zipOr . (visible *** (reverse . visible))
  transOr = concatMap zipOr
  trues = length . filter id
  zipOr = uncurry $ zipWith (||)
  toSnd f = id &&& f
  visible = _visible (-1)
  _visible max [] = []
  _visible max (x:xs) = if x > max then True : _visible x xs else False : _visible max xs
