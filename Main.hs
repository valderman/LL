module Main where

import System.Exit
import System.Environment (getArgs)

import LexMx
import ParMx
import PrintMx
import AbsMx

import Pretty
import ToLL (desugar)
import Erlang

import ErrM

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> run =<< getContents
        fs -> mapM_ (\ f -> run =<< readFile f) fs

run :: String -> IO ()
run s = case pProg (myLexer s) of
    Bad s -> do
        putStrLn $ "Parse failed:" ++ s ++ "!"
        exitWith (ExitFailure 2)
    Ok p -> case desugar p of
        Right d -> do
            putStrLn "== Pretty-Printed =="
            print d

            putStrLn "\n== Erlang Code =="
            print (compile d)
            exitSuccess
        Left e  -> print e >> exitFailure

