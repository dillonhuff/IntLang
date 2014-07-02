module Parser(
  parseExpr) where

import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim

import Lexer as Lex
import Syntax as Syn

parseExpr :: [Token] -> Expr
parseExpr toks = case parse expr "Parser" toks of
  Left err -> error $ show err
  Right expression -> expression
  
expr = buildExpressionParser table term

table =
  [[subtraction]]
  
subtraction = Infix (binop "-") AssocLeft

binop opName = do
  nameTok opName
  return $ bop opName
  
bop opName arg1 arg2 = ap (ap (var opName) arg1) arg2
  
term = numberTok

numberTok = do
  nt <- numTok
  return $ Syn.num $ numVal nt

nameTok :: (Monad m) => String -> ParsecT [Token] u m Token
nameTok name = ilTok (hasName name)

numTok :: (Monad m) => ParsecT [Token] u m Token
numTok = ilTok isNum

ilTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
ilTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position