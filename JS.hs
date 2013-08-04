{-# LANGUAGE GADTs, FlexibleInstances #-}
-- | Compile LL into JS.
module JS (pretty, compile, compileTo) where
import Data.Hashable
import LL hiding (var, subst, ax)

type Comment = String
data Var = V Name Comment | BlackHole | Scrutinee deriving (Eq, Ord)

var :: Name -> JS
var n = Var $ V n n

class Pretty a where
  pretty :: a -> String

data JS where
  Let      :: Var -> JS -> JS -> JS
  Read     :: JS -> Var -> JS -> JS
  Write    :: JS -> JS -> JS -> JS
  TryWrite :: JS -> JS -> JS -> JS
  Fork     :: JS -> JS -> JS
  Match    :: JS -> [(Lit, JS)] -> JS
  Stop     :: JS
  Arr      :: [JS] -> JS
  Index    :: JS -> JS -> JS
  Var      :: Var -> JS
  Ext      :: String -> JS
  Lit      :: Lit -> JS
  Die      :: JS

traverse :: (JS -> JS) -> JS -> JS
traverse f js =
    case js of
      Let v ex body     -> f $ Let v (tr ex) (tr body)
      Read inp v next   -> f $ Read (tr inp) v (tr next)
      Write out ex next -> f $ Write (tr out) (tr ex) (tr next)
      TryWrite o ex nxt -> f $ TryWrite (tr o) (tr ex) (tr nxt)
      Fork l r          -> f $ Fork (tr l) (tr r)
      Match scrut alts  -> f $ Match (tr scrut) (map (fmap tr) alts)
      Stop              -> f Stop
      Arr xs            -> f $ Arr $ map tr xs
      Index arr ix      -> f $ Index (tr arr) (tr ix)
      Var v             -> f $ Var v
      Ext s             -> f $ Ext s
      Lit l             -> f $ Lit l
      Die               -> f Die
  where
    tr = traverse f

data Lit where
  LNum :: Double -> Lit

right :: Lit
right = LNum 0

left :: Lit
left = LNum 1

newVar :: JS
newVar = Ext "s.newV()"

inVar :: JS
inVar = Ext "inVar"

outVar :: JS
outVar = Ext "outVar"

blackHole :: Var
blackHole = V "" ""

instance Pretty JS where
  pretty (Let v ex next) =
    "var " ++ pretty v ++ " = " ++ pretty ex ++ ";\n" ++ pretty next
  pretty (Read var var' next) =
    "s.read(" ++ pretty var ++ ", function(" ++ pretty var' ++ "){\n"
      ++ pretty next ++ "});\n"
  pretty (Write var x next) =
    "s.write(" ++ pretty var ++ "," ++ pretty x ++ ", function() {"
      ++ pretty next ++ "});\n"
  pretty (TryWrite var x next) =
    "s.tryWrite(" ++ pretty var ++ "," ++ pretty x ++ ", function() {"
      ++ pretty next ++ "});\n"
  pretty (Fork a b) =
    "s.spawn(function(){\n" ++ pretty a ++ "});\n" ++ pretty b
  pretty (Match scrut alts) =
    "switch(" ++ pretty scrut ++ "){\n" ++ prettyList "\n" alts ++ "}\n"
  pretty (Stop) =
    "return;\n"
  pretty (Arr xs) =
    "[" ++ prettyList "," xs ++ "]"
  pretty (Index arr ix) =
    pretty arr ++ "[" ++ pretty ix ++ "]"
  pretty (Ext f) =
    f
  pretty (Var v) =
    pretty v
  pretty (Lit l) =
    pretty l
  pretty (Die) =
    "throw 'blarg!';"

instance Pretty (Lit, JS) where
  pretty (c, body) = "case " ++ pretty c ++ ":\n" ++ pretty body ++ "break;\n"

prettyList sep (x:xs) = foldl (\a x -> a ++ sep ++ pretty x) (pretty x) xs
prettyList _ _      = ""

instance Pretty Var where
  -- Holy crap this is stupid, but I don't have the time for a proper name
  -- supply right now.
  pretty (V n c) | not $ okJSId n = 'v' : show (abs $ hash n) ++ comment
    where comment = if null c then "" else " /* " ++ c ++ " */"
  pretty (V n _) = n
  pretty (Scrutinee) = "scrut"
  pretty (BlackHole) = "_"

okJSId :: String -> Bool
okJSId (c:cs) = c `elem` jsIdChars && all (`elem` jsIdTailChars) cs
okJSId _      = False

jsIdChars = "_$" ++ ['a'..'z'] ++ ['A'..'Z']
jsIdTailChars = ['0'..'9'] ++ jsIdChars

instance Pretty Lit where
  pretty (LNum n) = show n

-- Turn b into a everywhere
subst a b = traverse s
  where
    s (Var (V v _)) | v == b = (Var $ V a a)
    s v                      = v

ax a b =
  Let (V "r" "") newVar $
  Fork (oneSide a b (var "r")) $ 
  Fork (oneSide b a (var "r")) $
  Read (var "r") (V "x" "") $
  Write (Index (var "x") (Lit $ LNum 0)) (Index (var "x") (Lit $ LNum 1)) $
  Write (Index (var "x") (Lit $ LNum 0)) (Index (var "x") (Lit $ LNum 1)) $
  Stop

oneSide from to result =
  Read from (V "x" "") $
  Write result (Arr [to, var "x"]) $
  Stop

compile :: Deriv -> JS
compile (Deriv t v s) =
    foldSeq (\_ -> comp) t v s
  where
    comp = SeqFinal {
        sxchg = const id,
        sty   = \_ _ -> (),
        szero = \_ _ -> Die,
        sbot  = \x -> Write (var x) (Arr []) $ Stop,
        sone = \_ x next -> Read (var x) BlackHole next,
        spar = \_ out x _ y _ l r ->
          Let (V x x) newVar $
          Let (V y y) newVar $
          Write (var out) (Arr [var x, var y]) $
          Fork l r,
        splus = \inp lv _ rv _ lalt ralt ->
          Read (var inp) Scrutinee $
          Match (Index (Var Scrutinee) (Lit $ LNum 0)) [
              (right, Let (V lv lv) (Index (Var Scrutinee) (Lit$LNum 1)) $ lalt),
              (left, Let (V rv rv) (Index (Var Scrutinee) (Lit$LNum 1)) $ ralt)
            ],
        swith = \_ _ l out chan _ next ->
          Let (V chan chan) newVar $
          Write (var out) (Arr [if l then Lit left else Lit right, var chan]) $
          next,
        scross = \_ inp first _ second _ next ->
          Read (var inp) Scrutinee $
          Let (V first first) (Index (Var Scrutinee) (Lit $ LNum 0)) $
          Let (V second second) (Index (Var Scrutinee) (Lit $ LNum 1)) $
          next,
        scut = \a b _ l _ r ->
          Let (V a a) newVar $
          subst a b $
          Fork l r,
        sax = \a b _ -> ax (var a) (var b),
        stapp = \_ a _ b _ next -> subst a b next,
        stunpack = \_ a b next -> subst a b next,
        -- Since JS does GC whether we want it to or not, we just skip the
        -- refcounting here.
        salias = \_ a b _ next -> subst a b next,
        signore = \_ _ next -> next,
        sdemand = \from v _ next -> Read (var from) (V v v) next,
        soffer = error "offer not done yet! :(",
        swhat = error "What?!",
        smem = error "mem",
        schplus = error "schplus",
        schcross = error "schcross",
        schfull = error "schfull",
        schtyp = error "schtyp",
        schpar = error "schpar",
        schempty = error "schempty"
      }

-- | Compile a derivation into a JS file with the necessary RTS stuff included.
compileTo :: FilePath -> Deriv -> IO ()
compileTo f d = do
    rts <- readFile "rts.js"
    writeFile f rts
    appendFile f $ "s.run(function(){\n" ++ comp d ++ "\n});"
  where
    comp = pretty . compile
