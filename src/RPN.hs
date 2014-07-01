module RPN(
  RPN, arg, intVal, funcall, appl) where

data RPN
     = Arg Int
     | IntVal Int
     | Funcall String Int
     | Appl
       deriving (Eq, Ord, Show)
       
arg = Arg
intVal = IntVal
funcall = Funcall
appl = Appl

