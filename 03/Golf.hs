module Golf where

import           Data.Bool
import           Data.List

skips :: [a] -> [[a]]
skips = filter (not . null) . runSkip
  where
    runSkip [] = []
    runSkip xs = [xs, snds xs] ++ nth xs
    nth xs     = foldr (\a b -> [a] : b) [] (drop 2 xs)
    snds xs    = foldr fsnd [] (zip xs [1..])
    fsnd x y   = bool y (fst x : y) (even $ snd x)
