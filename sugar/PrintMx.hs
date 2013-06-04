{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintMx where

-- pretty-printer generated by the BNF converter

import AbsMx
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print Id where
  prt _ (Id (_,i)) = doc (showString ( i))
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Prog where
  prt i e = case e of
   Deriv aliass tyvars binders seq -> prPrec i 0 (concatD [prt 0 aliass , prt 0 tyvars , prt 0 binders , doc (showString "|-") , prt 0 seq])


instance Print Alias where
  prt i e = case e of
   TyAlias id type' -> prPrec i 0 (concatD [doc (showString "type") , prt 0 id , doc (showString "=") , prt 0 type'])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print TyVar where
  prt i e = case e of
   TyVar id -> prPrec i 0 (concatD [prt 0 id])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Binder where
  prt i e = case e of
   Binder id type' -> prPrec i 0 (concatD [prt 0 id , doc (showString ":") , prt 0 type'])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print MBinder where
  prt i e = case e of
   BJust id type' -> prPrec i 0 (concatD [prt 0 id , doc (showString ":") , prt 0 type'])
   BNothing id -> prPrec i 0 (concatD [prt 0 id])


instance Print Along where
  prt i e = case e of
   AJust ids -> prPrec i 0 (concatD [doc (showString "along") , prt 0 ids])
   ANothing  -> prPrec i 0 (concatD [])


instance Print Type where
  prt i e = case e of
   Tensor type'0 type' -> prPrec i 4 (concatD [prt 4 type'0 , doc (showString "*") , prt 5 type'])
   Par type'0 type' -> prPrec i 4 (concatD [prt 4 type'0 , doc (showString "|") , prt 5 type'])
   One  -> prPrec i 5 (concatD [doc (showString "1")])
   Bot  -> prPrec i 5 (concatD [doc (showString "_|_")])
   Plus type'0 type' -> prPrec i 3 (concatD [prt 3 type'0 , doc (showString "+") , prt 4 type'])
   Choice type'0 type' -> prPrec i 3 (concatD [prt 3 type'0 , doc (showString "&") , prt 4 type'])
   Top  -> prPrec i 5 (concatD [doc (showString "T")])
   Zero  -> prPrec i 5 (concatD [doc (showString "0")])
   Lollipop type'0 type' -> prPrec i 2 (concatD [prt 0 type'0 , doc (showString "-o") , prt 2 type'])
   TyId id -> prPrec i 5 (concatD [prt 0 id])
   Bang type' -> prPrec i 5 (concatD [doc (showString "!") , prt 5 type'])
   Quest type' -> prPrec i 5 (concatD [doc (showString "?") , prt 5 type'])
   Neg type' -> prPrec i 5 (concatD [doc (showString "~") , prt 5 type'])
   Forall id type' -> prPrec i 0 (concatD [doc (showString "forall") , prt 0 id , doc (showString ".") , prt 0 type'])
   Exists id type' -> prPrec i 0 (concatD [doc (showString "exists") , prt 0 id , doc (showString ".") , prt 0 type'])


instance Print Choice where
  prt i e = case e of
   Fst  -> prPrec i 0 (concatD [doc (showString "fst")])
   Snd  -> prPrec i 0 (concatD [doc (showString "snd")])


instance Print Seq where
  prt i e = case e of
   Ax id0 id -> prPrec i 0 (concatD [prt 0 id0 , doc (showString "<->") , prt 0 id])
   Cut mbinder0 seq1 mbinder seq -> prPrec i 0 (concatD [doc (showString "connect") , doc (showString "{") , prt 0 mbinder0 , doc (showString "->") , prt 0 seq1 , doc (showString ";") , prt 0 mbinder , doc (showString "->") , prt 0 seq , doc (showString "}")])
   ParSeq id0 id1 seq2 id seq -> prPrec i 0 (concatD [doc (showString "connect") , doc (showString "via") , prt 0 id0 , doc (showString "{") , prt 0 id1 , doc (showString "->") , prt 0 seq2 , doc (showString ";") , prt 0 id , doc (showString "->") , prt 0 seq , doc (showString "}")])
   TensorSeq id0 id1 id seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString ",") , prt 0 id1 , doc (showString "=") , prt 0 id , doc (showString "in") , prt 0 seq])
   ChoiceSeq id0 choice id seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString "=") , prt 0 choice , prt 0 id , doc (showString "in") , prt 0 seq])
   Case id0 id1 seq2 id seq -> prPrec i 0 (concatD [doc (showString "case") , prt 0 id0 , doc (showString "of") , doc (showString "{") , doc (showString "inl") , prt 0 id1 , doc (showString "->") , prt 0 seq2 , doc (showString ";") , doc (showString "inr") , prt 0 id , doc (showString "->") , prt 0 seq , doc (showString "}")])
   Bottom id -> prPrec i 0 (concatD [prt 0 id])
   Unit id seq -> prPrec i 0 (concatD [doc (showString "let") , doc (showString "()") , doc (showString "=") , prt 0 id , doc (showString "in") , prt 0 seq])
   Crash id along -> prPrec i 0 (concatD [doc (showString "crash") , prt 0 id , prt 0 along])
   Pack id0 id type' seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString "=") , prt 0 id , doc (showString "@") , prt 0 type' , doc (showString "in") , prt 0 seq])
   Unpack id0 id1 id seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString "@") , prt 0 id1 , doc (showString "=") , prt 0 id , doc (showString "in") , prt 0 seq])
   Offer id0 id seq -> prPrec i 0 (concatD [doc (showString "offer") , prt 0 id0 , doc (showString "for") , prt 0 id , doc (showString "in") , prt 0 seq])
   Demand id0 id seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString "=") , doc (showString "demand") , prt 0 id , doc (showString "in") , prt 0 seq])
   Ignore id seq -> prPrec i 0 (concatD [doc (showString "ignore") , prt 0 id , doc (showString "in") , prt 0 seq])
   Alias id0 id seq -> prPrec i 0 (concatD [doc (showString "let") , prt 0 id0 , doc (showString "=") , doc (showString "alias") , prt 0 id , doc (showString "in") , prt 0 seq])
   Hole  -> prPrec i 0 (concatD [doc (showString "_")])



