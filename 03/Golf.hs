module Golf where

import           Data.Bool
import           Data.List

-----------------------------------------------------------------------------
-- 1.
skips :: [a] -> [[a]]
skips = filter (not . null) . runSkip
  where
    runSkip [] = []
    runSkip xs = [xs, snds xs] ++ nth xs
    nth xs     = foldr (\a b -> [a] : b) [] (drop 2 xs)
    snds xs    = foldr fsnd [] (zip xs [1..])
    fsnd x y   = bool y (fst x : y) (even $ snd x)

-----------------------------------------------------------------------------
-- 2.
localMaxima :: [Int] -> [Int]
localMaxima xs = sort $ go xs []
  where
    go [] ys         = ys
    go (x:[]) ys     = ys
    go (x:y:[]) ys   = ys
    go (x:y:z:xs) ys = go (y:z:xs) (bool ys (y:ys) (y > x && y > z))

-----------------------------------------------------------------------------
-- 3.
-- histogram :: [Int] -> String
histogram xs = fmap (\n -> (n, length $ filter (\x -> n == x) xs)) [1..9]
