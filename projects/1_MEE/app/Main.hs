module Main where

import Lexer (tokenize)
main :: IO ()
main = do
  print (tokenize "x + 3.14 * (y ^ 5)")