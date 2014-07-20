module Lexer(
  Token,
  strToToks,
  name, num,
  dname, dnum, dlp, drp, ddef, das, dtrue, dfalse, dif, dthen, delse,
  drecord, dis, drb, dlb, dcomma,
  isBuiltinOp, isName, isNum, isBool, hasName, pos,
  numVal, nameVal, boolVal) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

languageDef =
  emptyDef { Tok.commentStart    = "/*",
             Tok.commentEnd      = "*/",
             Tok.commentLine     = "//",
             Tok.identStart      = lower,
             Tok.identLetter     = alphaNum,
             Tok.reservedNames   = [ "if", "then", "else", "as", "def", "is", "record"],
             Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "||", "&&", "~"] }

lexer = Tok.makeTokenParser languageDef

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
  "<" -> True
  ">" -> True
  "<=" -> True
  ">=" -> True
  "==" -> True
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
delimiter = Delim
def = Res "def"
as = Res "as"
res = Res

-- Dummy token creation functions
dummyPos = newPos "DUMMY" 0 0

dname str = Name str dummyPos

dnum val = Num val dummyPos

dlp = Delim "(" dummyPos
drp = Delim ")" dummyPos
dlb = Delim "{" dummyPos
drb = Delim "}" dummyPos
dcomma = Delim "," dummyPos

ddef = Res "def" dummyPos
das = Res "as" dummyPos
dif = Res "if" dummyPos
dthen = Res "then" dummyPos
delse = Res "else" dummyPos
drecord = Res "record" dummyPos
dis = Res "is" dummyPos
dnil = Res "nil" dummyPos

dtrue = Boolean True dummyPos
dfalse = Boolean False dummyPos
-------------------------------------

strToToks :: String -> [Token]
strToToks str = case parse (sepBy tok spaces) "Lexer" str of
  Left err -> error $ show err
  Right toks -> toks

tok :: Parser Token
tok = varOrRes
      <|> number
      <|> delim
      <|> builtinOp
      <|> booleanValue

varOrRes :: Parser Token
varOrRes = try var <|> resWord

resWord :: Parser Token
resWord = do
  pos <- getPosition
  resStr <- try (string "if")
            <|> string "is"
            <|> string "then"
            <|> string "else"
            <|> string "as"
            <|> string "def"
            <|> string "record"
  return $ res resStr pos
  
varName = Tok.identifier lexer

var :: Parser Token
var = do
  pos <- getPosition
  first <- varName
  return $ name first pos
  
oneCharVar :: Parser String
oneCharVar = do
  x <- lower
  return $ (x:[])
  
manyCharVar :: Parser String
manyCharVar = do
  x <- lower
  rest <- many1 alphaNum
  return $ (x:rest)
  
number :: Parser Token
number = do
  pos <- getPosition
  nums <- many1 digit
  return $ num (read nums :: Int) pos
  
delim :: Parser Token
delim = do
  pos <- getPosition
  d <- string "(" 
       <|> string ")"
       <|> string "{" 
       <|> string "}"
       <|> string ","
  return $ delimiter d pos

builtinOp :: Parser Token
builtinOp = do
  pos <- getPosition
  op <- try (string "==")
        <|> try (string "<=")
        <|> try (string ">=")
        <|> string "<"
        <|> string ">"
        <|> string "+" 
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