{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import MarXup.MultiRef
import Reductions
import Data.Monoid
import Symheap
import TexPretty
import LL
import AM
import Rules
import DiagPretty
import Control.Monad

preamble :: Tex ()
preamble = do
  usepackage ["utf8"] "inputenc"
  usepackage [] "graphicx" -- used for import metapost diagrams
  usepackage [] "amsmath"
  usepackage [] "amssymb" -- extra symbols such as □
  usepackage [] "cmll" -- for the operator "par"
  usepackage ["a4paper","margin=2cm"] "geometry"
  cmd "input" (tex "unicodedefs")
  title "Compiling LL to Erlang"
  authorinfo Plain [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch)
                   ,("Josef Svenningsson","",ch)
                   ,("Dan Rosén","danr@chalmers.se",ch)
                   ]
 where ch = ""

-- | Render a derivation tree.
deriv :: Bool -- ^ Show terms?
         -> Deriv -> Tex Label
deriv showProg (Deriv tvs vs s) = derivationTree [] $ texSeq showProg tvs vs s

-- | Render a derivation tree, showing terms.
deriv' = deriv True

-- | Render a derivation as a program (term)
program :: Deriv -> Tex ()
program (Deriv tvs vs s) = treeRender (texProg tvs vs s)

rul s s' = displayMath $ cmdn "frac" [block[diagSystem s,texSystem s], block[texSystem s',diagSystem s']] >> return ()

toSystem' h =  toSystem h . fillTypes

comment :: Tex a -> TeX
comment x = ""

allPosTypes = [One,Zero,tA:⊕:tB,tA:⊗:tB,Bang tA,Forall "α" tAofAlpha]


allRules =
  [[(axRule, "Copy the data between the closures; when it's ready.")]
  ,[(cutRule, "similar to ⅋ but connects the two closures directly together.")]
  ,[(parRule, "split the environment and spawn a new closure. (No communication)"),
    (crossRule, "add an entry in the context for @tB, at location @math{n + @mkLayout(tA)} (No communication)")]
  ,[(withRule True,"Write the tag and the pointer. Deallocate if the other possibility uses less memory."),
    (plusRule,"wait for the data to be ready; then chose a branch according to the tag. Free the pointer and the tag.  Free the non-used part of the disjunction.")]
  ,[(botRule,"terminate (delete the closure)"),
    (oneRule,"continue")]
  ,[(zeroRule,"crash")]
  {-
  ,[(forallRule,"Write the (pointer to) representation of the concrete type @tB (found in the code) to the 1st cell.")
   ,(existsRule,existComment)]
  ,[(questRule,@"place a pointer to the closure @math{a} in the zone pointed by @math{x:A}, mark as ready; terminate.@"),
    (bangRule,@"wait for ready. Allocate and initialise memory of @mkLayout(tA), spawn a closure from
                  the zone pointed by @math{x:!A}, link it with @math{x} and  continue. Decrement reference count.@")]
  ,[(weakenRule,"discard the pointer, decrement reference count. Don't forget about recursively decrementing counts upon deallocation.")]
  ,[(contractRule,"copy the pointer to the thunk, increment reference count.  Note this is easy in the AM compared to the cut-elim.")]
  -}
  ]

existComment = @"Wait for the type representation to be ready. Copy the
  (pointer to) the representation to the type environment. Free the
  type variable from the memory. Rename the linear variable (as in
  ⊗). NOTE: It is tempting to avoid the sync. point here and instead have
  one when the type-variable is accessed. However
  because the type variable will be copied around (when a closure is spawned),
  then this rule must be responsible for freeing the memory (or we need garbage collection;
  yuck).@"

-- | Print all derivation rules
typeRules = figure "Compilation of the derivations" $
    forM_ (concat allRules) $ \ r -> do
        math (deriv'' r)
        newline
        cmd0 "vspace{5em}"

deriv'' (x,_) = deriv' x

operationalRules = itemize $ forM_ allRules $ amRule' emptyHeap

amRule' :: SymHeap -> [(Deriv,TeX)] -> TeX
amRule' _ [] = ""
amRule' h0 ((seq,comment):seqs) = do
  item
  @"Rule: @seqName(derivSequent seq) @"
  displayMath $ deriv' seq
  comment
  newline
  case msys1 of
     Nothing -> cmd "text" "no rule for" <> program seq
     Just sys1 -> rul sys0 sys1 >> amRule' (snd sys1) seqs

  where sys0 = toSystem' h0 seq
        msys1 = stepSystem sys0

-- | Render abstract machine rules
amRule = amRule' emptyHeap



allReductions displayer = env "center" $ mapM_ redRule $
   [
    ("AxCut",cutAx),
    (math par<>"⊗",cutParCross),
    (amp<>"⊕",cutWithPlus True),
    ("?!", cutBang),
    ("⊥!",cutUnit),
    ("∃∀",cutQuant),
    ("?Contract",cutContract),
    ("?Weaken",cutIgnore)
    ]
   ++ pushRules

  where redRule (name,input) = do
          name
          newline
          cmd0 "vspace{3pt}"
          cmd "fbox" $ math $ do
            displayer input
            cmd0 "Longrightarrow"
            displayer (eval input)
          newline
          cmd0 "vspace{1em}"

todo = cmd "marginpar"

pole :: TeX
pole = "⊥" <> tex "{\\kern -1ex}" <> "⊥"

instance Element Type where
  type Target Type = TeX
  element = texClosedType

instance Element Layout where
  type Target Layout = TeX
  element = math . texLayout

tA = meta "A"
tB = meta "B"
arr = meta "a"
keys = meta "k"
vals = meta "v"

norm :: TeX -> TeX
norm x = math $ "|" <> x <> "|"

nothing _ = mempty

main = render $ latexDocument "article" ["10pt"] preamble $ @"

@typeRules

@"



