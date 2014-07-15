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
   (bool True, [boolVal True]),
   (bool False, [boolVal False]),
   (num 3, [intVal 3]),
   (ap (ap (var "+") (num 3)) (num 5), [intVal 5, intVal 3, funcall "int_add" 2, appl, appl]), 
   (var "+", [funcall "int_add" 2]),
   (var "-", [funcall "int_sub" 2]),
   (var "*", [funcall "int_mul" 2]),
   (var "/", [funcall "int_div" 2]),
   (var "||", [funcall "bool_or" 2]),
   (var "&&", [funcall "bool_and" 2]),
   (var "~", [funcall "bool_not" 1])]