module Syntax(
  Expr, FDef, RecordDef, ProgramDefs,
  ilRecordDef, fdef, ap, var, num, bool, ite,
  programDefs, toRPN, builtinMap,
  numFields, accessors, constructor) where

import Control.Monad.State.Lazy hiding (ap)
import Data.Map as M

import RPN

type RecordField = String

data RecordDef = RecordDef String [RecordField]
                 deriving (Eq, Ord, Show)

ilRecordDef = RecordDef

numFields (RecordDef _ fields) = length fields

accessors (RecordDef _ fields) = zip fields [0..(length fields - 1)]

constructor (RecordDef name _) = name

data FDef = FDef String Int
            deriving (Eq, Ord, Show)

fdef = FDef
arity (FDef _ a) = a
name (FDef n _) = n

data ProgramDefs = ProgramDefs {
  functionDefs       :: Map String FDef,
  argumentNums       :: Map String Int,
  accessorIndexes    :: Map String Int,
  constructorArities :: Map String Int
  }

programDefs = ProgramDefs

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

toRPN :: Map String FDef -> Map String Int -> Map String Int -> Map String Int -> Expr -> [RPN]
toRPN funcDefs argNums accessorInds constructorArts expr = fst $ toRPNWithLabelNums 0 programDefs expr
  where
    programDefs = ProgramDefs funcDefs argNums accessorInds constructorArts

toRPNWithLabelNums :: Int -> ProgramDefs -> Expr -> ([RPN], Int)
toRPNWithLabelNums n _ (Num v) = ([intVal v], n)
toRPNWithLabelNums n _ (Boolean b) = ([boolVal b], n)
toRPNWithLabelNums n pDefs (Ap l r) = (rightRPN ++ leftRPN ++ [appl], nr)
  where
    leftRPNLab = toRPNWithLabelNums n pDefs l
    leftRPN = fst leftRPNLab
    nl = snd leftRPNLab
    rightRPNLab = toRPNWithLabelNums nl pDefs r
    rightRPN = fst rightRPNLab
    nr = snd rightRPNLab
toRPNWithLabelNums n pDefs (Var v) = case M.lookup v (argumentNums pDefs) of
  Just argNum -> ([arg argNum], n)
  Nothing -> case M.lookup v (functionDefs pDefs) of
      Just (FDef name arity) -> ([funcall name arity], n)
      Nothing -> case M.lookup v (constructorArities pDefs) of
        Just arity -> ([intVal arity, funcall "create_record" (arity + 1), appl], n)
        Nothing -> case M.lookup v (accessorIndexes pDefs) of
          Just index -> ([intVal index, funcall "get_field" 2, appl], n)
          Nothing -> error $ v ++ " is not defined\nDefined functions are " ++ show (functionDefs pDefs)
toRPNWithLabelNums n pDefs (IfThenElse e1 e2 e3) = (finalRPN, snd e3RPNLab)
  where
    e1RPNLab = toRPNWithLabelNums (n+2) pDefs e1
    e1RPN = fst e1RPNLab
    e2RPNLab = toRPNWithLabelNums (snd e1RPNLab) pDefs e2
    e2RPN = fst e2RPNLab
    e3RPNLab = toRPNWithLabelNums (snd e2RPNLab) pDefs e3
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