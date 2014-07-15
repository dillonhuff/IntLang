module Syntax(
  Expr, FDef,
  fdef, ap, var, num, bool,
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
       Var String   |
       Num Int      |
       Boolean Bool
       deriving (Eq, Ord, Show)

ap = Ap
var = Var
num = Num
bool = Boolean

toRPN :: Map String FDef -> Map String Int -> Expr -> [RPN]
toRPN _ _ (Num v) = [intVal v]
toRPN _ _ (Boolean b) = [boolVal b]
toRPN fMap vMap (Ap l r) = rightRPN ++ leftRPN ++ [appl]
  where
    leftRPN = toRPN fMap vMap l
    rightRPN = toRPN fMap vMap r
toRPN funcMap varMap (Var v) = case M.lookup v varMap of
  Just argNum -> [arg argNum]
  Nothing -> case M.lookup v funcMap of
      Just (FDef name arity) -> [funcall name arity]
      Nothing -> error $ v ++ " is not defined\nDefined functions are " ++ show funcMap

builtinMap = M.fromList [("+", fdef "int_add" 2),
                         ("-", fdef "int_sub" 2),
                         ("*", fdef "int_mul" 2),
                         ("/", fdef "int_div" 2),
                         ("||", fdef "bool_or" 2),
                         ("&&", fdef "bool_and" 2),
                         ("~", fdef "bool_not" 1)]