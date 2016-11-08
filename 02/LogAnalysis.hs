{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

-- import Log

splitToLines :: String -> [String]
splitToLines x = lines x

splitFirst :: [String] -> [String]
splitFirst xs = [take 1 x | x <- xs]

splitHead :: String -> [String]
splitHead xs = splitFirst . splitToLines $ xs

parseType :: String -> String
parseType []  = "Nothing br0"
parseType "E" = "Error"
parseType "W" = "Warning"
parseType "I" = "Info"
parseType _   = "Nothing Againz"

parse :: String -> [String]
parse = map parseType . splitHead