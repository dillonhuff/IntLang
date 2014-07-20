module Parser(
  parseProgram,
  parseExpr,
  parseRecord) where

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Lexer as Lex
import Program
import Syntax as Syn

parseProgram :: [Token] -> ILProgram
parseProgram toks = case parse ilProg "Program Parser" toks of
  Left err -> error $ show err
  Right p -> p

data ILProgComponent = ILF ILFunction
                     | ILR RecordDef
                       deriving (Show)
                                
isFunc (ILF _) = True
isFunc _ = False

isRecDec (ILR _) = True
isRecDec _ = False

ilf (ILF f) = f

ilr (ILR r) = r

ilProg = do
  progComps <- many (programComponent)
  let funcs = map ilf $ filter isFunc progComps
      recDecs = map ilr $ filter isRecDec progComps in
    return $ ilProgram funcs recDecs
  
programComponent = funcComp <|> recordDefComp

funcComp = do
  func <- ilFunction
  return $ ILF func
  
recordDefComp = do
  recDef <- record
  return $ ILR recDef

ilFunction = do
  defTok
  name <- anyNameTok
  args <- many anyNameTok
  asTok
  body <- expr
  return $ ilFunc (nameVal name) (map nameVal args) body
  
parseRecord :: [Token] -> RecordDef
parseRecord toks = case parse record "Record Parser" toks of
  Left err -> error $ show err
  Right recDec -> recDec

record = do
  recordTok
  name <- anyNameTok
  lBracket
  fields <- sepBy anyNameTok commaTok
  rBracket
  return $ ilRecordDef (nameVal name) (map nameVal fields)

parseExpr :: [Token] -> Expr
parseExpr toks = case parse expr "Expr Parser" toks of
  Left err -> error $ show err
  Right expression -> expression
  
expr = buildExpressionParser table term

table =
  [[logicalNegation],
   [multiplication, division],
   [addition, subtraction],
   [lessThan, greaterThan, lessThanOrEqual, greaterThanOrEqual],
   [equal],
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

-- Comparison builtins
equal = Infix (binaryOp "==") AssocLeft
lessThan = Infix (binaryOp "<") AssocLeft
greaterThan = Infix (binaryOp ">") AssocLeft
lessThanOrEqual = Infix (binaryOp "<=") AssocLeft
greaterThanOrEqual = Infix(binaryOp ">=") AssocLeft

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
       <|> ifThenElseSt

funcArg = try (parens builtinOperator)
          <|> parens expr
          <|> numberTok
          <|> namedTok
          <|> booleanTok

parens e = do
  lParen
  x <- e
  rParen
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
  
ifThenElseSt = do
  ifTok
  condE <- expr
  thenTok
  thenE <- expr
  elseTok
  elseE <- expr
  return $ ite condE thenE elseE

builtinOperator = do
  t <- builtinOpTok
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
ifTok = ilTok (== dif)
thenTok = ilTok (== dthen)
elseTok = ilTok (== delse)
recordTok = ilTok (== drecord)

lParen = ilTok (== dlp)
rParen = ilTok (== drp)
lBracket = ilTok (== dlb)
rBracket = ilTok (== drb)
commaTok = ilTok (== dcomma)

anyNameTokOtherThan forbiddenNames = ilTok (\t -> isName t && (not $ Prelude.elem t forbiddenNames))

anyNameTok :: (Monad m) => ParsecT [Token] u m Token
anyNameTok = ilTok (\t -> isName t && (not $ isBuiltinOp t))

builtinOpTok = ilTok isBuiltinOp

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