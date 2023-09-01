{- I used vis(1) SRE's to generate the folder structure.
 - After that I ran this horrible program. -}
module Day7 where

import Control.Lens (unsnoc)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Traversable as T
import System.Directory.Tree

data NewTree a = NewDir FileName [NewTree a] Integer
               | NewFile Integer deriving (Show, Functor, Foldable)

p1 = sum . filter ((>=) 100000) <$> solve

p2 = minimum . uncurry go . fromJust . unsnoc . sort . filter ((/=) 0) <$> solve where
  go xs root = map (\x -> if ((70000000 - root) + x >= 30000000) then x else 70000000) xs

solve = sizes . op <$> walk

walk = do
  _:/ tree <- readDirectoryWith return "day7/"
  return tree

op (Dir n c) = NewDir n (map op c) (foldr further 0 $ concatMap flattenDir c)
op (File n f) = NewFile $ (read :: String -> Integer) n

further (File n f) acc = (read :: String -> Integer) n + acc
further (Dir _ c) acc = acc

sizes (NewDir _ xs size) = [size] ++ concatMap sizes xs
sizes (NewFile _) = [0]
I 
