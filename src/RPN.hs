module RPN(
  RPN, arg, intVal, funcall, appl, toCCode) where

import CCodeGen

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

toCCode :: RPN -> CStatement
toCCode (Arg argNum) = pushArgOnStack argNum
toCCode (IntVal n) = pushIntOnStack n
toCCode (Funcall name arity) = pushFuncOnStack name arity