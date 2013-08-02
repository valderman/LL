{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Erll.Compile (compile, compileDeriv, preludeCompile) where

import LL hiding ((&),var,Alias,Demand,Ignore)
import Erll.Syn
import Erll.Unique
import Erll.Prelude

import Text.PrettyPrint
import Data.List (find)


-- | Compile a derivation to Erlang
compile :: Deriv -> Code'
compile = compileDeriv . erlangUnique

-- | Compile a derivation to Erlang, without renamings
compileDeriv :: Deriv -> Code'
compileDeriv (Deriv ts vs sq) = foldSeq sf ts vs sq
  where
    sf :: Deriv -> SeqFinal () Code'
    sf = const SeqFinal {..}
      where
        sxchg _ c = c

        sax a b _ = Axiom a b

        scut x y _ cx _ cy = chunk
            [ Var x := NewChannel
            , (x // y) (cx ||| cy)
            ]

        scross _b z x _ y _ c = Var x & Var y := Ask z |> c

        spar _b z x _ y _ cx cy = chunk
            [ Var x := NewChannel
            , Var y := NewChannel
            , z ! Var x & Var y
            , cx ||| cy
            ]

        swith _b _ b z x _ c = chunk
            [ Var x := NewChannel
            , z ! (if b then InL else InR) & Var x
            , c
            ]

        splus z x _ y _ cx cy = Case (Ask z)
            [ (InL & Var x,cx)
            , (InR & Var y,cy)
            ]

        sbot x = x ! tt

        sone _b x c = tt := Ask x |> c

        szero _ _ = Crash

        stapp _b z _ x _ c = (z // x) c

        stunpack _ z x c = (z // x) c

        soffer _b z x _ c = Promote z x c

        sdemand z x _ c = Var x := Demand z |> c

        signore z _ c = Ignore z |> c

        salias _b z z' _ c = Alias z |> (z // z') c

        swhat z xs = error $ "compile: What " ++ z ++ "[" ++ unwords xs ++ "]‽"

        sty _ _ = ()

        ~[sfold, sunfold, smem, schplus, schcross, schpar, schtyp, schempty, schfull] =[]

preludeCompile :: Prelude -> String -> Deriv -> String
preludeCompile Prelude{..} m d =
    let d'@(Deriv _ vs _) = erlangUnique d
        bifs = getBifs (map fst vs) prel_bifs
        c = Spawn (compileDeriv d')
    in  (unlines
            [ "% Compile with:"
            , "% erl -compile " ++ m
            , "% Run with:"
            , "% erl -pa ./ -run " ++ m ++ " main -run init stop -noshell"
            , "-module(" ++ m ++ ")."
            , "-export([main/0])."
            ])
        ++ prel_str ++
        (render $ hang "main() ->" 4 $ vcat $
            [ pp (Var n := NewChannel) <> comma
            | (n,_,_) <- bifs
            ] ++
            concat [ map text s | (_,Init,s) <- bifs ] ++
            [ pp c <> comma ] ++
            concat [ map text s | (_,Final,s) <- bifs ] ++
            ["ok."]
        )

getBifs :: [String] -> [Bif] -> [Bif]
getBifs (x:xs) bifs = case find (\ (y,_,_) -> x == y) bifs of
    Just bif -> bif:getBifs xs bifs
    Nothing  -> error $ x ++ " is not defined as a built-in-function"
getBifs [] _ = []

