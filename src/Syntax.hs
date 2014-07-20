module Syntax(
  Expr, FDef, RecordDef,
  ilRecordDef, fdef, ap, var, num, bool, ite,
  toRPN, builtinMap) where

import Control.Monad.State.Lazy hiding (ap)
import Data.Map as M

import RPN

type RecordField = String

data RecordDef = RecordDef String [RecordField]
                 deriving (Eq, Ord, Show)

ilRecordDef = RecordDef

data FDef = FDef String Int
            deriving (Eq, Ord, Show)

fdef = FDef
arity (FDef _ a) = a
name (FDef n _) = n

data Expr
     = Ap Expr Expr
     | Var String
     | Num Int
     | Boolean Bool
     | IfThenElse Expr Expr Expr
       deriving (Eq, Ord, Show)

ap = Ap
var = Var
num = Num
bool = Boolean
ite = IfThenElse
        
toRPN :: Map String FDef -> Map String Int -> Expr -> [RPN]
toRPN funcDefs argNums expr = fst $ toRPNWithLabelNums 0 funcDefs argNums expr

toRPNWithLabelNums :: Int -> Map String FDef -> Map String Int -> Expr -> ([RPN], Int)
toRPNWithLabelNums n _ _ (Num v) = ([intVal v], n)
toRPNWithLabelNums n _ _ (Boolean b) = ([boolVal b], n)
toRPNWithLabelNums n fMap vMap (Ap l r) = (rightRPN ++ leftRPN ++ [appl], nr)
  where
    leftRPNLab = toRPNWithLabelNums n fMap vMap l
    leftRPN = fst leftRPNLab
    nl = snd leftRPNLab
    rightRPNLab = toRPNWithLabelNums nl fMap vMap r
    rightRPN = fst rightRPNLab
    nr = snd rightRPNLab
toRPNWithLabelNums n funcMap varMap (Var v) = case M.lookup v varMap of
  Just argNum -> ([arg argNum], n)
  Nothing -> case M.lookup v funcMap of
      Just (FDef name arity) -> ([funcall name arity], n)
      Nothing -> error $ v ++ " is not defined\nDefined functions are " ++ show funcMap
toRPNWithLabelNums n funcMap varMap (IfThenElse e1 e2 e3) = (finalRPN, snd e3RPNLab)
  where
    e1RPNLab = toRPNWithLabelNums (n+2) funcMap varMap e1
    e1RPN = fst e1RPNLab
    e2RPNLab = toRPNWithLabelNums (snd e1RPNLab) funcMap varMap e2
    e2RPN = fst e2RPNLab
    e3RPNLab = toRPNWithLabelNums (snd e2RPNLab) funcMap varMap e3
    e3RPN = fst e3RPNLab
    finalRPN = e1RPN ++ [jumpFalse n] ++ e2RPN ++ [jump (n+1)] ++ [label n] ++ e3RPN ++ [label (n+1)]

builtinMap = M.fromList [("+", fdef "int_add" 2),
                         ("-", fdef "int_sub" 2),
                         ("*", fdef "int_mul" 2),
                         ("/", fdef "int_div" 2),
                         ("||", fdef "bool_or" 2),
                         ("&&", fdef "bool_and" 2),
                         ("~", fdef "bool_not" 1),
                         (">", fdef "greater" 2),
                         ("<", fdef "less" 2),
                         (">=", fdef "greater_or_equal" 2),
                         ("<=", fdef "less_or_equal" 2),
                         ("==", fdef "equal" 2)]