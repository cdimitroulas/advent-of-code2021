module AOC.Pretty where

-- Makes a string which will be printed in red
mkRed :: String -> String
mkRed str = "\x1b[31m" <> str <> "\x1b[0m"
