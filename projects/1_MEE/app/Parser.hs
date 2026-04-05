module Parser where

import Types

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr [] = Left "Empty Token List"
parseExpr tokens =
  case parseTerm tokens of
    Left err -> Left err
    Right (left, rest) ->
      case rest of
        (TPlus : rest2) ->
          case parseTerm rest2 of
            Left err -> Left err
            Right (right, rest3) -> Right (BinOp Add left right, rest3)
        (TMinus : rest2) ->
          case parseTerm rest2 of
            Left err -> Left err
            Right (right, rest3) -> Right (BinOp Sub left right, rest3)
        _ -> Right (left, rest)

parseTerm :: [Token] -> Either String (Expr, [Token])
parseTerm [] = Left "Empty Token List"
-- parseTerm tokens = 
--   case parseFactor tokens of
--     Left err -> Left err
--     Right (right, rest) ->

parseFactor :: [Token] -> Either String (Expr, [Token])
parseFactor [] = Left "Empty Token List"
-- parseFactor tokens = 
--   case parseAtom tokens of
--     Left err -> Left err
--     Right ()

parseAtom :: [Token] -> Either String (Expr, [Token])
parseAtom [] = Left "unexpected end of expression"
parseAtom (TNum n : rest) = Right (Num n, rest)
parseAtom (TVar s : rest) = Right (Var s, rest)
parseAtom (TMinus : rest) = do
  (expr, rest2) <- parseAtom rest
  Right (Neg expr, rest2)
parseAtom (TLParen : rest) = do
  (expr, rest2) <- parseExpr rest
  case rest2 of
    (TRParen : rest3) -> Right (expr, rest3)
    _                 -> Left "Missing closing parenthesis"
parseAtom _ = Left "Unexpected token"