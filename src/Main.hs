module Main(main) where

import System.Environment
import System.IO

import CCodeGen
import Lexer
import Parser
import Program

main :: IO ()
main = do
  (fileName:rest) <- getArgs
  fileHandle <- openFile fileName ReadMode
  programText <- hGetContents fileHandle
  let compiledProg = compile programText
  putStrLn compiledProg
  
compile :: String -> String
compile = show . toCProgram . parseProgram . strToToks
