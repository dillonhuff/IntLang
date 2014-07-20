module SyntaxTests(
  allSyntaxTests) where

import Data.Map as M

import RPN
import Syntax
import TestUtils

allSyntaxTests = do
  testFunction (toRPN builtinMap varMap accessorMap constructorMap) toRPNCases
  
varMap = M.fromList [("v", 1), ("d", 2), ("e", 3)]

accessorMap = M.fromList [("data", 0), ("next", 1), ("val", 0), ("left", 1), ("right", 2)]

constructorMap = M.fromList [("list", 2), ("bTree", 3)]
  
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
   (var "~", [funcall "bool_not" 1]),
   (var ">", [funcall "greater" 2]),
   (var "<", [funcall "less" 2]),
   (var ">=", [funcall "greater_or_equal" 2]),
   (var "<=", [funcall "less_or_equal" 2]),
   (var "==", [funcall "equal" 2]),
   (ite (ap (var "~") (var "e")) (num (-4)) (var "d"),
    [arg 3, funcall "bool_not" 1, appl, jumpFalse 0, intVal (-4), jump 1, label 0, arg 2, label 1]),
   (var "list", [intVal 2, funcall "create_record" 2]),
   (var "bTree", [intVal 3, funcall "create_record" 3]),
   (var "data", [intVal 0, funcall "get_field" 2]),
   (var "next", [intVal 1, funcall "get_field" 2]),
   (var "val", [intVal 0, funcall "get_field" 2]),
   (var "left", [intVal 1, funcall "get_field" 2]),
   (var "right", [intVal 2, funcall "get_field" 2])]