module SysStepper where

import AM
import Pretty
import Symheap
import System.Random


iStepper :: System SymHeap -> IO ()
iStepper sys = do putStrLn $ showSystem sys
                  iLoop sys

iLoop sys = do putStrLn (helpText (noClosures sys - 1))
               c <- getChar
               case c of
                 'q' -> return ()
                 'p' -> cont (Just sys)
                 'r' -> do c <- randomRIO (0,noClosures sys - 1)
                           cont (stepClosure c sys)
                 'f' -> cont (stepSystem sys)
                 n | n >= '0' && fromEnum n - fromEnum '0' <= noClosures sys ->
                   cont (stepClosure (fromEnum c - fromEnum '0') sys)
                 c -> do putStrLn $ "Unrecognized command " ++ show c
                         iLoop sys
  where cont Nothing = do putStrLn "No step possible"
                          iLoop sys
        cont (Just sys') = do putStrLn $ showSystem sys'
                              iLoop sys'

helpText n = "0-" ++ show n ++ ": select a closure to run; r: random closure; f: first steppable closure; p: print current system; q: quit."
