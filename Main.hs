module Main where

import System.Exit
import System.Environment (getArgs)

import LexMx
import ParMx
import LayoutMx (resolveLayout)
import PrintMx hiding (render)
import AbsMx hiding (Deriv)

import Pretty
import ToLL (desugar)
import Erlang
import LL (Deriv(..),eval)

import Control.Monad

import Text.PrettyPrint (render)
import Text.Show.Pretty

import ErrM

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> run =<< getContents
        fs -> mapM_ (run <=< readFile) fs

run :: String -> IO ()
run s = case pProg (resolveLayout False (myLexer s)) of
    Bad s -> do
        putStrLn $ "Parse failed:" ++ s ++ "!"
        exitWith (ExitFailure 2)
    Ok p -> do
        let (r,_msg) = desugar p
        -- putStrLn (render msg)
        case r of
            Right ds -> do
                forM_ ds $ \ d -> do
                    putStrLn $ ppAttempt $ replace '⊕' '+' $ replace '⊗' '*' $ show d
                    putStrLn "== Pretty-Printed =="
                    putStrLn (showDeriv d)

                    -- putStrLn "\n== Erlang Code =="
                    -- print (compile d)
                exitSuccess
            Left e  -> print e >> exitFailure

ppAttempt :: String -> String
ppAttempt s = maybe s valToStr (parseValue s)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\ x -> if x == a then b else x)

