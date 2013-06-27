{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module PaperData where

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
import Reductions
import Data.Monoid
import Symheap
import TexPretty
import LL
import AM
import Rules
import DiagPretty
import Control.Monad
import GraphViz
import Framework
import Data.List


chanRules :: [(Deriv,TeX)]               
chanRules =   
  [(chanPlusRule True,  "A channel containing a bit")
  ,(chanPlusRule False, "A channel containing a bit")
--  ,(chanCrossRule,     "A half-split channel (par side)")
--  ,(chanParRule,       "A half-split channel (par side)")
--  ,(chanTypRule,       "A channel containing a type")
--  ,(chanEmptyRule 3,   "A memory cell (empty)")
--  ,(chanFullRule 3,    "A memory cell (full)")
  ]

texBosons :: Tex SortedLabel
texBosons = figure "Rules mediating interaction" $ 
    env "center" $ do
    forM_ chanRules $ \(r,_comment) -> do 
        math $ deriv False r
        cmd0 "hspace{1em}"

typesetBosonReds reds = env "center" $ 
    forM_ reds $ \(name,input) -> math $ do
      sequent input
      cmd0 "Longrightarrow"
      sequent (eval' input)
      
      return ()

texBosonReds =  figure "Asynchronous reduction rules" $ 
                env "center" $ 
                typesetBosonReds chanRedRules