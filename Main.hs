{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs hiding (verbosity,auto)
import System.Process

import System.Exit
import System.Environment (getArgs)

import LexMx
import ParMx
import LayoutMx (resolveLayout)
import PrintMx hiding (render)
import AbsMx hiding (Deriv)

import Pretty
import ToLL (desugar)
import Erlang as E
import LL (Deriv(..),eval)

import Control.Monad

import ErlangPrelude

import Text.PrettyPrint (render)
import Text.Show.Pretty

import ErrM

import System.FilePath

data Params = Params
    { file    :: FilePath
    , make    :: Bool
    , compile :: Bool
    , run     :: Bool
    , debug   :: Bool
    }
  deriving (Data,Typeable)

defParams :: Params
defParams = Params
    { file    = ""    &= argPos 0 &= typFile
    , make    = False &= help "Make .erl file"
    , compile = False &= help "Compile .erl file"
    , run     = False &= help "Run .erl file"
    , debug   = False &= help "Print some debugging information"
    }

main :: IO ()
main = do
    params@Params{..} <- cmdArgs defParams
    runFile params =<< readFile file

runFile :: Params -> String -> IO ()
runFile Params{..} s = case pProg (resolveLayout False (myLexer s)) of
    Bad s -> do
        putStrLn $ "Parse failed:" ++ s ++ "!"
        exitWith (ExitFailure 2)
    Ok p -> do
        let (r,_msg) = desugar p
        -- putStrLn (render msg)
        case r of
            Right ds -> do
                forM_ ds $ \ d -> do
                    when debug $ putStrLn $ ppAttempt $ replace '⊕' '+' $ replace '⊗' '*' $ show d
                    when (not make) $ do
                        putStrLn "== Pretty-Printed =="
                        putStrLn (showDeriv d)
                    when debug $ do
                        putStrLn "== Erlang Code =="
                        print (E.compile d)

                when make $ do
                    prel_str <- readFile "prelude.erl"
                    let prel  = parsePrelude prel_str
                        modul = takeBaseName file
                        erl   = modul <.> "erl"
                    when debug $ putStrLn $ ppAttempt $ show prel
                    let erl_str = preludeCompile prel modul (last ds)
                    when debug $ putStrLn erl_str
                    putStrLn $ "Writing " ++ erl
                    writeFile erl erl_str
                    when compile $ do
                        void $ systemShow $ "erl -compile " ++ modul
                        when run $ void $ systemShow $
                            "erl -pa ./ -run " ++ modul ++ " main -run init stop -noshell"
                exitSuccess
            Left e  -> print e >> exitFailure

systemShow :: String -> IO ExitCode
systemShow s = do
    putStrLn $ "Running " ++ s
    system s


ppAttempt :: String -> String
ppAttempt s = maybe s valToStr (parseValue s)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\ x -> if x == a then b else x)

