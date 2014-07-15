module LexerTests(
  allLexerTests) where

import Lexer
import TestUtils

allLexerTests = do
  testFunction strToToks lexCases
  
lexCases =
  [("def", [ddef]),
   ("as", [das]),
   ("if", [dif]),
   ("then", [dthen]),
   ("else", [delse]),
   ("(", [dlp]),
   (")", [drp]),
   ("True", [dtrue]),
   ("False", [dfalse]),
   ("+", [dname "+"]),
   ("-", [dname "-"]),
   ("*", [dname "*"]),
   ("/", [dname "/"]),
   ("&&", [dname "&&"]),
   ("||", [dname "||"]),
   ("~", [dname "~"]),
   ("<", [dname "<"]),
   (">", [dname ">"]),
   ("<=", [dname "<="]),
   (">=", [dname ">="]),
   ("==", [dname "=="]),
   ("nooo", [dname "nooo"]),
   ("e34Di3", [dname "e34Di3"]),
   ("iflle", [dname "iflle"]),
   ("elseor", [dname "elseor"]),
   ("definition", [dname "definition"]),
   ("1 - 45", [dnum 1, dname "-", dnum 45]),
   ("x + 4", [dname "x", dname "+", dnum 4])]