module RPN(
  RPN, arg, intVal, boolVal, funcall, appl, toCCode) where

import CCodeGen

data RPN
     = Arg Int
     | IntVal Int
     | Funcall String Int
     | Appl
     | BoolVal Bool
       deriving (Eq, Ord, Show)
       
arg = Arg
intVal = IntVal
boolVal = BoolVal
funcall = Funcall
appl = Appl

toCCode :: RPN -> CStatement
toCCode (Arg argNum) = pushArgOnStack argNum
toCCode (IntVal n) = pushIntOnStack n
toCCode (BoolVal b) = pushBoolOnStack b
toCCode (Funcall name arity) = pushFuncOnStack name arity
toCCode Appl = bind