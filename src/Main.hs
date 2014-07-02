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
  putStrLn programText
  let compiledProg = compile programText
      outFileName = cFileName fileName
  writeFile outFileName compiledProg
  hClose fileHandle
  putStrLn compiledProg
  
compile :: String -> String
compile = show . toCProgram . parseProgram . strToToks

parsedProg = parseProgram . strToToks

cFileName :: String -> String
cFileName name = (takeWhile (/='.') name) ++ ".c"