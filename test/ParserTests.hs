module ParserTests(
  allParserTests) where

import Lexer as Lex
import Parser
import Syntax as Syn
import TestUtils

allParserTests = do
  testFunction (parseExpr . strToToks) parseExprCases
  
parseExprCases =
  [("1", Syn.num 1),
   ("1 - 4", ap (ap (var "-") (Syn.num 1)) (Syn.num 4)),
   ("3 * 3", ap (ap (var "*") (Syn.num 3)) (Syn.num 3)),
   ("5 / 873", ap (ap (var "/") (Syn.num 5)) (Syn.num 873)),
   ("x / 4", ap (ap (var "/") (var "x")) (Syn.num 4)),
   ("1 + 7", ap (ap (var "+") (Syn.num 1)) (Syn.num 7)),
   ("2 /(4 + 2)",
    ap (ap (var "/") (Syn.num 2)) (ap (ap (var "+") (Syn.num 4)) (Syn.num 2))),
   ("f 2 y", ap (ap (var "f") (Syn.num 2)) (var "y")),
   ("f 2 (+) 1", ap (ap (ap (var "f") (Syn.num 2)) (var "+")) (Syn.num 1)),
   ("f (2 *4) / 4",
    ap (ap (var "/") (ap (var "f") (ap (ap (var "*") (Syn.num 2)) (Syn.num 4)))) (Syn.num 4)),
   ("True", bool True),
   ("False", bool False),
   ("~True", ap (var "~") (bool True)),
   ("f True (~False)", ap (ap (var "f") (bool True)) (ap (var "~") (bool False))),
   ("True || False", ap (ap (var "||") (bool True)) (bool False)),
   ("False && x", ap (ap (var "&&") (bool False)) (var "x")),
   ("x || ~(y && z)",
    ap (ap (var "||") (var "x")) (ap (var "~") (ap (ap (var "&&") (var "y")) (var "z"))))]