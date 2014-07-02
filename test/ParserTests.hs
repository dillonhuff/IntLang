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
   ("1 - 4", ap (ap (var "-") (Syn.num 1)) (Syn.num 4))]



