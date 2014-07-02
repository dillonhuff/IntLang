module Syntax(
  Expr, FDef,
  fdef, ap, func, var, num,
  toRPN, builtinMap) where

import Data.Map as M

import RPN

data FDef = FDef String Int
            deriving (Eq, Ord, Show)

fdef = FDef
arity (FDef _ a) = a
name (FDef n _) = n

data Expr
     = Ap Expr Expr |
       Func String  |
       Var String   |
       Num Int
       deriving (Eq, Ord, Show)

ap = Ap
func = Func
var = Var
num = Num

toRPN :: Map String FDef -> Map String Int -> Expr -> [RPN]
toRPN fMap vMap (Ap l r) = rightRPN ++ leftRPN ++ [appl]
  where
    leftRPN = toRPN fMap vMap l
    rightRPN = toRPN fMap vMap r
toRPN _ varMap (Var v) = case M.lookup v varMap of
  Just argNum -> [arg argNum]
  Nothing -> error $ v ++ " is not an argument of the current function"
toRPN _ _ (Num n) = [intVal n]
toRPN fDefs _ (Func fName) = [funcall impFuncName impFuncArity]
  where
    funcDef = case M.lookup fName fDefs of
      Just fdef -> fdef
      Nothing -> error $ fName ++ " is not a defined function"
    impFuncName = name funcDef
    impFuncArity = arity funcDef

builtinMap = M.fromList [("+", fdef "int_add" 2),
                         ("-", fdef "int_sub" 2),
                         ("*", fdef "int_mul" 2),
                         ("/", fdef "int_div" 2)]