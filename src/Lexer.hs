module Lexer(
  Token,
  strToToks,
  name, num,
  dname, dnum, dlp, drp, ddef, das,
  isName, isNum, hasName, pos,
  isLP, isRP,
  numVal, nameVal) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos

import qualified Text.Parsec.Token as Tok

data Token
     = Name String SourcePos
     | Num Int SourcePos
     | LP SourcePos
     | RP SourcePos
     | DEF SourcePos
     | AS SourcePos
       deriving (Show)
                
instance Eq Token where
  (==) (Name n1 _) (Name n2 _) = n1 == n2
  (==) (Num n1 _) (Num n2 _) = n1 == n2
  (==) (LP _) (LP _) = True
  (==) (RP _) (RP _) = True
  (==) (DEF _) (DEF _)  = True
  (==) (AS _) (AS _) = True
  (==) _ _ = False

pos :: Token -> SourcePos
pos (Name _ p) = p
pos (Num _ p) = p
pos (LP p) = p
pos (RP p) = p
pos (DEF p) = p
pos (AS p) = p

hasName name (Name n _) = name == n
hasName _ _ = False

isName (Name _ _) = True
isName _ = False

isNum (Num _ _) = True
isNum _ = False

isLP (LP _) = True
isLP _ = False

isRP (RP _) = True
isRP _ = False

nameVal (Name n _) = n
nameVal t = error $ show t ++ " has no name"

numVal (Num n _) = n
numVal t = error $ show t ++ " is not an integer"

name = Name
num = Num
lp = LP
rp = RP
def = DEF
as = AS

dname str = Name str (newPos "DUMMY" 0 0)
dnum val = Num val (newPos "DUMMY" 0 0)
dlp = LP (newPos "DUMMY" 0 0)
drp = RP (newPos "DUMMY" 0 0)
ddef = DEF (newPos "DUMMY" 0 0)
das = AS (newPos "DUMMY" 0 0)

strToToks :: String -> [Token]
strToToks str = case parse (endBy tok spaces) "Lexer" str of
  Left err -> error $ show err
  Right toks -> toks
  
tok :: Parser Token
tok = try resWord <|> try funcOrVar <|> try number <|> try delim <|> builtinOp

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
  op <- string "+"  <|> string "-" <|> string "*" <|> string "/"
  return $ name op pos