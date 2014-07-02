module Parser(
  parseProgram,
  parseExpr) where

import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Prim

import Lexer as Lex
import Program
import Syntax as Syn

parseProgram :: [Token] -> ILProgram
parseProgram toks = case parse ilProg "Program Parser" toks of
  Left err -> error $ show err
  Right p -> p

ilProg = do
  progFuncs <- many ilFunction
  return $ ilProgram progFuncs
  
ilFunction = do
  defTok
  name <- anyNameTok
  args <- many anyNameTok
  asTok
  body <- (expr args)
  return $ ilFunc (nameVal name) (map nameVal args) body

parseExpr :: [Token] -> [Token] -> Expr
parseExpr varToks toks = case parse (expr varToks) "Parser" toks of
  Left err -> error $ show err
  Right expression -> expression
  
expr varToks = buildExpressionParser table (term varToks)

table =
  [[multiplication, division],
   [addition, subtraction]]
  
addition = Infix (binop "+") AssocLeft
subtraction = Infix (binop "-") AssocLeft
multiplication = Infix (binop "*") AssocLeft
division = Infix (binop "/") AssocLeft

binop opName = do
  nameTok opName
  return $ bop opName
  
bop opName arg1 arg2 = ap (ap (var opName) arg1) arg2

term varList = parens (expr varList) <|> (funcAp varList)  <|> numberTok <|> namedTok

funcArg varList = parens (expr varList) <|> numberTok <|> namedTok

parens e = do
  lparen
  x <- e
  rparen
  return x

numberTok = do
  nt <- numTok
  return $ Syn.num $ numVal nt
  
namedTok = do
  t <- anyNameTok
  return $ var $ nameVal t
  
funcAp varList = do
  funcName <- anyNameTokOtherThan varList
  args <- many (funcArg varList)
  let fName = nameVal funcName
  return $ application (var fName) args
  
application :: Expr -> [Expr] -> Expr
application e [] = e
application e (x:xs) = application (ap e x) xs

defTok = ilTok isDef
asTok = ilTok isAs

lparen = ilTok isLP
rparen = ilTok isRP

anyNameTokOtherThan forbiddenNames = ilTok (\t -> isName t && (not $ Prelude.elem t forbiddenNames))

anyNameTok :: (Monad m) => ParsecT [Token] u m Token
anyNameTok = ilTok isName

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