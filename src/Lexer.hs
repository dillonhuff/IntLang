module Lexer(
  Token,
  strToToks,
  name, num,
  dname, dnum, dlp, drp, ddef, das, dtrue, dfalse,
  isBuiltinOp, isName, isNum, isBool, hasName, pos,
  numVal, nameVal, boolVal) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

import qualified Text.Parsec.Token as Tok

data Token
     = Name String SourcePos
     | Num Int SourcePos
     | Boolean Bool SourcePos
     | Res String SourcePos
     | Delim String SourcePos
       deriving (Show)
                
instance Eq Token where
  (==) (Name n1 _) (Name n2 _) = n1 == n2
  (==) (Num n1 _) (Num n2 _) = n1 == n2
  (==) (Boolean b1 _) (Boolean b2 _) = b1 == b2
  (==) (Res n1 _) (Res n2 _) = n1 == n2
  (==) (Delim n1 _) (Delim n2 _) = n1 == n2
  (==) _ _ = False

pos :: Token -> SourcePos
pos (Name _ p) = p
pos (Num _ p) = p
pos (Boolean _ p) = p
pos (Res _ p) = p
pos (Delim _ p) = p

isBuiltinOp (Name n _) = case n of
  "*" -> True
  "-" -> True
  "+" -> True
  "/" -> True
  "||" -> True
  "&&" -> True
  "~" -> True
  _ -> False
isBuiltinOp _ = False

hasName name (Name n _) = name == n
hasName _ _ = False

isName (Name _ _) = True
isName _ = False

isNum (Num _ _) = True
isNum _ = False

isBool (Boolean _ _) = True
isBool _ = False

nameVal (Name n _) = n
nameVal t = error $ show t ++ " has no name"

numVal (Num n _) = n
numVal t = error $ show t ++ " is not an integer"

boolVal (Boolean b _) = b
boolVal t = error $ show t ++ " is not a boolean"

name = Name
num = Num
lp = Delim "("
rp = Delim ")"
def = Res "def"
as = Res "as"

dummyPos = newPos "DUMMY" 0 0

dname str = Name str dummyPos
dnum val = Num val dummyPos
dlp = Delim "(" dummyPos
drp = Delim ")" dummyPos
ddef = Res "def" dummyPos
das = Res "as" dummyPos
dtrue = Boolean True dummyPos
dfalse = Boolean False dummyPos

strToToks :: String -> [Token]
strToToks str = case parse (endBy tok spaces) "Lexer" str of
  Left err -> error $ show err
  Right toks -> toks
  
tok :: Parser Token
tok = try resWord
      <|> try funcOrVar
      <|> try number
      <|> try delim
      <|> try builtinOp
      <|> booleanValue

resWord :: Parser Token
resWord = resDef <|> resAs


resDef = do
  pos <- getPosition
  x <- string "def"
  return $ def pos

resAs = do
  pos <- getPosition
  x <- string "as"
  return $ as pos

funcOrVar :: Parser Token
funcOrVar = do
  pos <- getPosition
  first <- lower
  rest <- many alphaNum
  return $ name (first:rest) pos
  
number :: Parser Token
number = do
  pos <- getPosition
  nums <- many1 digit
  return $ num (read nums :: Int) pos
  
delim :: Parser Token
delim = lParen <|> rParen

lParen = do
  pos <- getPosition
  lpar <- char '('
  return $ lp pos
  
rParen = do
  pos <- getPosition
  rpar <- char ')'
  return $ rp pos
  
builtinOp :: Parser Token
builtinOp = do
  pos <- getPosition
  op <- string "+" 
        <|> string "-"
        <|> string "*"
        <|> string "/"
        <|> string "||"
        <|> string "&&"
        <|> string "~"
  return $ name op pos
  
booleanValue :: Parser Token
booleanValue = do
  pos <- getPosition
  boolVal <- string "True" <|> string "False"
  return $ Boolean (read boolVal :: Bool) pos