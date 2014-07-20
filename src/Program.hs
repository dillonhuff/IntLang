module Program(
  toCProgram,
  ILProgram, ilProgram,
  ILFunction, ilFunc) where

import Data.Map as M

import CCodeGen
import RPN
import Syntax

data ILProgram = Prog [ILFunction] [RecordDef]
                 deriving (Show)

ilProgram = Prog

toCProgram :: ILProgram -> CProgram
toCProgram (Prog funcs recDefs) = cProgram defaultImports prototypes funcDefs
  where
    defaultImports = [cInclude "BasicRuntime.h"]
    prototypes = Prelude.map makePrototype funcs
    userDefFuncsMap = M.fromList $ zip (Prelude.map fName funcs) (Prelude.map mkFDef funcs)
    funcDefMap = M.union builtinMap userDefFuncsMap
    constructors = constructorMap recDefs
    accessors = accessorMap recDefs
    funcDefs = Prelude.map (toCFunc funcDefMap constructors accessors) funcs

constructorMap :: [RecordDef] -> Map String Int
constructorMap recDefs = M.fromList $ zip (Prelude.map constructor recDefs) (Prelude.map numFields recDefs)

accessorMap :: [RecordDef] -> Map String Int
accessorMap recDefs = M.fromList $ concat $ Prelude.map accessors recDefs

data ILFunction = ILF { fName :: String, fArgNames :: [String],  fBody :: Expr }
                  deriving (Show)

ilFunc :: String -> [String] -> Expr -> ILFunction
ilFunc name argNames body = ILF name argNames body

toCFunc :: Map String FDef -> Map String Int -> Map String Int -> ILFunction -> CFunction
toCFunc fDefs conArities accInds (ILF name argNames expr) = cFunc name body
  where
    vMap = M.fromList $ zip argNames [1..]
    body = Prelude.map toCCode $ toRPN fDefs vMap accInds conArities expr
    
    
mkFDef :: ILFunction -> FDef
mkFDef (ILF fn args _) = fdef fn (length args)

makePrototype :: ILFunction -> CFuncDeclaration
makePrototype (ILF name _ _) = stdDec name