module Types (Op (..), Expr (..), Token (..)) where

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show)

data Expr
  = Num Double
  | Var String
  | BinOp Op Expr Expr
  | Neg Expr
  deriving (Show)

data Token
  = TVar String
  | TNum Double
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TPow
  | TAssign
  | TLParen
  | TRParen
  deriving (Show)
