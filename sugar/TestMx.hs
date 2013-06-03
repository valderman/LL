module Main where

import System.Exit
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexMx
import ParMx
import SkelMx
import PrintMx
import AbsMx

import Pretty
import ToLL (desugar)

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Prog -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Prog -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree
                          case desugar tree of
                            Right d -> print d >> exitSuccess
                            Left e  -> print e >> exitFailure

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProg
            "-s":fs -> mapM_ (runFile 0 pProg) fs
            fs -> mapM_ (runFile 2 pProg) fs




