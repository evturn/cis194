{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage xs = let ys = words xs in
  case ys of
    ("I":x:rest)   -> LogMessage Info    (read x) (unwords rest)
    ("W":x:rest)   -> LogMessage Warning (read x) (unwords rest)
    ("E":y:x:rest) -> LogMessage (Error  (read y))
                                         (read x) (unwords rest)
    _              -> Unknown                     (unwords ys)

parse :: String -> [LogMessage]
parse = map parseMessage . lines