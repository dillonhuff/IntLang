module SyntaxTests(
  allSyntaxTests) where

import Data.Map as M

import RPN
import Syntax
import TestUtils

allSyntaxTests = do
  testFunction (toRPN builtinMap varMap) toRPNCases
  
varMap = M.fromList [("v", 1), ("d", 2), ("e", 3)]
  
toRPNCases =
  [(var "d", [arg 2]),
   (var "v", [arg 1]),
   (var "e",[arg 3]),
   (num 3, [intVal 3]),
   (ap (ap (func "+") (num 3)) (num 5), [intVal 5, intVal 3, funcall "int_add" 2, appl, appl]), 
   (func "+", [funcall "int_add" 2]),
   (func "-", [funcall "int_sub" 2]),
   (func "*", [funcall "int_mul" 2]),
   (func "/", [funcall "int_div" 2])]