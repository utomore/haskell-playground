module Lexer (tokenize) where

import Data.Char
import Types

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c : cs)
  | c == ' ' = tokenize cs
  | c == '+' = fmap (TPlus :) (tokenize cs)
  | c == '-' = fmap (TMinus :) (tokenize cs)
  | c == '*' = fmap (TMul :) (tokenize cs)
  | c == '/' = fmap (TDiv :) (tokenize cs)
  | c == '=' = fmap (TAssign :) (tokenize cs)
  | c == '^' = fmap (TPow :) (tokenize cs)
  | c == '(' = fmap (TLParen :) (tokenize cs)
  | c == ')' = fmap (TRParen :) (tokenize cs)
  | isDigit c =
      let (digits, rest) = span (\x -> isDigit x || x == '.') (c : cs)
       in fmap (TNum (read digits) :) (tokenize rest)
  | isAlpha c =
      let (variable, rest) = span (\x -> isAlpha x || isDigit x) (c : cs)
       in fmap (TVar variable :) (tokenize rest)
  | otherwise = Left ("unexpected:" ++ [c])