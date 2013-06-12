module Main where

import System.Exit
import System.Environment (getArgs)

import LexMx
import ParMx
import PrintMx hiding (render)
import AbsMx hiding (Deriv)

import Pretty
import ToLL (desugar)
import Erlang
import LL (Deriv(..))

import Text.PrettyPrint.HughesPJ (render)

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
    Ok p -> do
        let (r,msg) = desugar p
        putStrLn (render msg)
        case r of
            Right d@(Deriv _ _ s) -> do
                print s
                putStrLn "== Pretty-Printed =="
                print d

                putStrLn "\n== Erlang Code =="
                print (compile d)
                exitSuccess
            Left e  -> print e >> exitFailure

