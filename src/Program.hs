module Program(
  toCProgram,
  ILProgram, ilProgram,
  ILFunction, ilFunc) where

import Data.Map as M

import CCodeGen
import RPN
import Syntax

data ILProgram = Prog [ILFunction]

ilProgram = Prog

toCProgram :: ILProgram -> CProgram
toCProgram (Prog funcs) = cProgram defaultImports prototypes funcDefs
  where
    defaultImports = [cInclude "BasicRuntime.h"]
    prototypes = Prelude.map makePrototype funcs
    userDefFuncsMap = M.fromList $ zip (Prelude.map fName funcs) (Prelude.map mkFDef funcs)
    funcDefMap = M.union builtinMap userDefFuncsMap
    funcDefs = Prelude.map (toCFunc funcDefMap) funcs

data ILFunction = ILF { fName :: String, fArgNames :: [String],  fBody :: Expr }

ilFunc :: String -> [String] -> Expr -> ILFunction
ilFunc name argNames body = ILF name argNames body

toCFunc :: Map String FDef -> ILFunction -> CFunction
toCFunc fMap (ILF name argNames expr) = cFunc name (Prelude.map toCCode $ toRPN fMap vMap expr)
  where
    vMap = M.fromList $ zip argNames [1..]
    
mkFDef :: ILFunction -> FDef
mkFDef (ILF fn args _) = fdef fn (length args)

makePrototype :: ILFunction -> CFuncDeclaration
makePrototype (ILF name _ _) = stdDec name