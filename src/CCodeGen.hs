module CCodeGen(
  CProgram, cProgram,
  CStatement,
  CFunction, cFunc,
  CFuncDeclaration, stdDec,
  cInclude,
  pushArgOnStack, pushIntOnStack, pushFuncOnStack, pushBoolOnStack,
  bind, makeLabel, makeJump, makeJumpFalse) where

import Data.List

data CProgram
     = CProgram {
       imports        :: [CPreProcessStatement],
       funcPrototypes :: [CFuncDeclaration],
       funcDefs       :: [CFunction]
       }
       
instance Show CProgram where
  show (CProgram imps ps defs) = impStr ++ psStr ++ defStr
    where
      impStr = (stList "\n" (map show imps)) ++ "\n"
      psStr = stList ";\n" (map show ps) ++ "\n"
      defStr = stList "\n" (map show defs) ++ "\n"
       
cProgram = CProgram

argList [] = ""
argList (x:[]) = x
argList (x:rest) = x ++ ", " ++ argList rest

stList :: String -> [String] -> String
stList terminator sts = concat $ zipWith (++) sts (replicate (length sts) terminator)

data CPreProcessStatement
     = Include String
     | Define String String

instance Show CPreProcessStatement where
  show (Include name) = "#include " ++ name
  show (Define name val) = "#define " ++ name ++ " " ++ val       

cInclude str = Include (['\"'] ++ str ++ ['\"'])

data CFunction
     = CFunc {
       declaration :: CFuncDeclaration,
       body        :: [CStatement]
       }

instance Show CFunction where
  show (CFunc dec bod) = show dec ++ " {\n" ++ (stList ";\n" $ map show bod) ++ "}\n"

cFunc name body = CFunc (stdDec name) body

data CFuncDeclaration
     = CFuncDec {
       returnType :: String,
       name       :: String,
       args       :: [CVarDec]
       }

instance Show CFuncDeclaration where
  show (CFuncDec retty n ars) = retty ++ " " ++  n ++ "(" ++ decList ars ++ ")"

stdDec name = CFuncDec "void" name [(CVD "Comp *" "c")]

data CStatement = CFuncall String [String]
                | CLabel Int
                | JumpFalse Int
                | Jump Int

instance Show CStatement where
  show (CFuncall name args) = name ++ "(" ++ argList args ++ ")"
  show (CLabel n) = "L" ++ show n ++ ":"
  show (JumpFalse n) = "if (*((int*) (pop_stack()->result)) == 0) goto " ++ "L" ++ show n
  show (Jump n) = "goto " ++ "L" ++ show n

pushArgOnStack argNum = CFuncall "push_stack" ["nth_arg(c, " ++ show argNum ++ ")"]
pushIntOnStack n = CFuncall "push_int" [show n]
pushBoolOnStack True = CFuncall "push_int" [show 1]
pushBoolOnStack False = CFuncall "push_int" [show 0]
pushFuncOnStack name arity = CFuncall "push_func" [name, show arity]
bind = CFuncall "bind_ops" []
makeLabel n = CLabel n
makeJump n = Jump n
makeJumpFalse n = JumpFalse n

data CVarDec
     = CVD {
       typeName :: String,
       varName  :: String
       }
       
instance Show CVarDec where
  show (CVD tName vName) = tName ++ " " ++ vName
  
decList [] = ""
decList (vd:[]) = show vd
decList (vd:rest) = show vd ++ ", " ++ decList rest