module RPN(
  RPN,
  arg, intVal, boolVal, funcall, appl,
  jump, jumpFalse, label,
  toCCode) where

import CCodeGen

data RPN
     = Arg Int
     | IntVal Int
     | Funcall String Int
     | Appl
     | BoolVal Bool
     | Jump Int
     | JumpFalse Int
     | Label Int
       deriving (Eq, Ord, Show)
       
arg = Arg
intVal = IntVal
boolVal = BoolVal
funcall = Funcall
appl = Appl
jump = Jump
jumpFalse = JumpFalse
label = Label

toCCode :: RPN -> CStatement
toCCode (Arg argNum) = pushArgOnStack argNum
toCCode (IntVal n) = pushIntOnStack n
toCCode (BoolVal b) = pushBoolOnStack b
toCCode (Funcall name arity) = pushFuncOnStack name arity
toCCode Appl = bind
toCCode (Label n) = makeLabel n
toCCode (Jump n) = makeJump n
toCCode (JumpFalse n) = makeJumpFalse n