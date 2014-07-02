module LexerTests(
  allLexerTests) where

import Lexer
import TestUtils

allLexerTests = do
  testFunction strToToks lexCases
  
lexCases =
  [("def", [ddef]),
   ("(", [dlp]),
   (")", [drp]),
   ("+", [dname "+"]),
   ("-", [dname "-"]),
   ("*", [dname "*"]),
   ("/", [dname "/"]),
   ("nooo", [dname "nooo"]),
   ("e34Di3", [dname "e34Di3"])]