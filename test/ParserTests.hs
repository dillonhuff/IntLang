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
   ("1 + 7", ap (ap (var "+") (Syn.num 1)) (Syn.num 7)),
   ("2 /(4 + 2)", ap (ap (var "/") (Syn.num 2)) (ap (ap (var "+") (Syn.num 4)) (Syn.num 2))),
   ("f 2 y", ap (ap (var "f") (Syn.num 2)) (var "y")),
   ("f 2 + 1", ap (ap (ap (var "f") (Syn.num 2)) (var "+")) (Syn.num 1)),
   ("f (2 *4) / 4", ap (ap (ap (var "f") (ap (ap (var "*") (Syn.num 2)) (Syn.num 4))) (var "/")) (Syn.num 4))]



