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
  body <- expr
  return $ ilFunc (nameVal name) (map nameVal args) body

parseExpr :: [Token] -> Expr
parseExpr toks = case parse expr "Parser" toks of
  Left err -> error $ show err
  Right expression -> expression
  
expr = buildExpressionParser table term

table =
  [[logicalNegation],
   [multiplication, division],
   [addition, subtraction],
   [logicalAnd],
   [logicalOr]]
  
-- Arithmetic builtins
addition = Infix (binaryOp "+") AssocLeft
subtraction = Infix (binaryOp "-") AssocLeft
multiplication = Infix (binaryOp "*") AssocLeft
division = Infix (binaryOp "/") AssocLeft

-- Logical builtins
logicalNegation = Prefix (unaryOp "~")
logicalAnd = Infix (binaryOp "&&") AssocLeft
logicalOr = Infix (binaryOp "||") AssocLeft

binaryOp opName = do
  nameTok opName
  return $ bop opName
  
bop opName arg1 arg2 = ap (ap (var opName) arg1) arg2

unaryOp opName = do
  nameTok opName
  return $ unop opName
  
unop opName arg = ap (var opName) arg

term = parens expr
       <|> funcAp
       <|> numberTok
       <|> namedTok
       <|> booleanTok

funcArg = try (parens infixOperator)
          <|> parens expr
          <|> numberTok
          <|> namedTok
          <|> booleanTok

parens e = do
  lparen
  x <- e
  rparen
  return x

numberTok = do
  nt <- numTok
  return $ Syn.num $ numVal nt
  
booleanTok = do
  bt <- boolTok
  return $ bool $ boolVal bt
  
namedTok = do
  t <- anyNameTok
  return $ var $ nameVal t
  
infixOperator = do
  t <- infixTok
  return $ var $ nameVal t
  
funcAp = do
  funcName <- anyNameTok
  args <- many funcArg
  let fName = nameVal funcName
  return $ application (var fName) args
  
application :: Expr -> [Expr] -> Expr
application e [] = e
application e (x:xs) = application (ap e x) xs

defTok = ilTok (== ddef)
asTok = ilTok (== das)

lparen = ilTok (== dlp)
rparen = ilTok (== drp)

anyNameTokOtherThan forbiddenNames = ilTok (\t -> isName t && (not $ Prelude.elem t forbiddenNames))

anyNameTok :: (Monad m) => ParsecT [Token] u m Token
anyNameTok = ilTok (\t -> isName t && (not $ infixOp t))

infixTok = ilTok infixOp

nameTok :: (Monad m) => String -> ParsecT [Token] u m Token
nameTok name = ilTok (hasName name)

numTok :: (Monad m) => ParsecT [Token] u m Token
numTok = ilTok isNum

boolTok = ilTok isBool

ilTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
ilTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position