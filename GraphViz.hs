module GraphViz (couplingDiag) where
    
import qualified Text.PrettyPrint.HughesPJ as P
import LL
import TexPretty
import Pretty
import Control.Monad.RWS
import MarXup.Latex
import MarXup.Tex
import System.IO.Unsafe
import System.IO
import System.Process
import Data.List (intercalate)
import Control.Applicative


type GGen a = RWS () [String] Int a
type NodeRef = String
type Attr = (String,String)

mkAttrs :: [Attr] -> String
mkAttrs as = "[" ++ intercalate "," (map mkEq as)  ++ "]"
  where mkEq :: Attr -> String
        mkEq (x,y) = x ++ "=" ++ y

node :: [Attr] -> GGen NodeRef
node extraAttrs  = do
  let attrs = -- "fixedsize=true" : 
              -- "width=5" : 
              extraAttrs
  r <- get
  put (r+1)
  let ref = "nd" ++ show r 
  tell [ref ++ mkAttrs attrs ++ ";"]
  return ref
  
edge, edge' :: NodeRef -> NodeRef -> Type -> GGen ()

edge sn tn ty@(Meta True "Ξ" []) = edge' tn sn (neg ty)
edge sn tn ty@(Meta False _  _ ) = edge' tn sn (neg ty)
edge sn tn ty = edge' sn tn ty

edge' sn tn ty = do
  comment $ "  edge of type " ++ P.render (pClosedType ty)
  tell [sn ++ " -> " ++ tn ++ mkAttrs [("label",typ)
                                      -- ,("len","0.1") -- Does not seem to work; the edge label is placed as if len=1
                                      ] ++ ";"]
  where typ = dotQuote $ concat $ render $ texClosedTypeNoMath ty
  
dotQuote x = '"': concatMap esc x ++ "\""
  where esc '\\' = "\\\\"
        esc x = [x]
  
{-
rm :: Maybe Int -> [t] -> [t]
rm Nothing xs = xs        
rm (Just x) xs = l++r where (l,_:r) = splitAt x xs
-}
comment :: String -> GGen ()
comment x = tell $ ["// " ++ x]

emptyLab :: Attr
emptyLab = ("label",dotQuote " ")

toGraphPart' :: [Name] -> [(Name,Type)] -> Seq -> GGen [NodeRef]
toGraphPart' te e (Cut v v' ty γ s t) = do
  let (e0,e1) = splitAt γ e
  (s':sn) <- toGraphPart' te ((v,neg ty):e0) s
  (t':tn) <- toGraphPart' te ((v', ty)  :e1) t
  edge s' t' ty
  return $ sn ++ tn
toGraphPart' te e (Exchange π s) = do
  comment $ "  exchange"
  applyPerm π <$> toGraphPart' te (applyPerm π e) s  
toGraphPart' te e this@(Par True _ v v' x s t) = do
  let (v0,(_w,(vt :|: vt')):v1) = splitAt x e
  tn0 <- toGraphPart' te (v0++[(v,vt)]) s
  let tn = init tn0
      t' = last tn
  (s':sn) <- toGraphPart' te ((v',vt'):v1) t
  thisNode <- node $ getNodeLab this
  edge thisNode s' vt
  edge thisNode t' vt'
  return (sn++thisNode:tn)
toGraphPart' te e s@(Cross True _ v v' x t) = do
  let (v0,(_w,(vt :⊗: vt')):v1) = splitAt x e
  tn <- toGraphPart' te (v0++(v,vt):(v',vt'):v1) t 
  let (ts0,(t1:t2:ts3)) = splitAt x tn
  thisNode <- node $ getNodeLab s
  edge thisNode t1 vt
  edge thisNode t2 vt'
  return (ts0++thisNode:ts3)
toGraphPart' _te e s = do 
  comment $ "  leaf"
  thisNode <- node $ getNodeLab s
  return $ replicate (length e) thisNode
  
  
toGraphMain :: Deriv -> GGen ()
toGraphMain (Deriv te e s) = do
  hs <- toGraphPart'  te e s
  forM_ (zip hs e) $ \(n,(_,ty)) -> do
    hypNode <- node [("color","white"),emptyLab] -- $ "shape = " ++ show "none"
    edge hypNode n ty
    
  {-
type Port = Int
data HInfo = External Type | Internal NodeRef Type | Done


toGraphPart :: Maybe Port -> [Name] -> [(Name,HInfo)] -> Seq -> GGen NodeRef
toGraphPart parent te e (Cut v v' ty γ s t) = do
    comment $ "  cut"
    sn <- toGraphPart (Just 0) te ((v,Done):e0) s
    tn <- toGraphPart (Just 0) te ((v',Internal sn ty)   :e1) t
    return $ if maybe True (< γ) parent then sn else tn
  where (e0,e1) = splitAt γ e
-- toGraphPart parent te e (Cross True _ _ _ x         
toGraphPart parent te e (Exchange π s) = do
  comment $ "  exchange"
  toGraphPart ((`indexOf` π) `fmap` parent) te [e !! i | i <- π] s
toGraphPart _parent _te e s = do
  comment $ "  leaf"
  thisNode <- node $ getNodeLab s
  forM_ e $ \ (_name,h) -> do
    case h of
      Done -> return ()
      External ty -> do 
        -- hypNode <- node $ "shape = " ++ show "none"
        hypNode <- node ["color=white",emptyLab] -- $ "shape = " ++ show "none"
        edge hypNode thisNode ty
      Internal n ty -> edge n thisNode ty
  return thisNode    
  
toGraphMain :: Deriv -> GGen ()
toGraphMain (Deriv te e s) = do
  toGraphPart Nothing te [(nm,External ty) | (nm,ty) <- e] s
  return ()
-}

getNodeLab :: Seq -> [Attr]
getNodeLab (What x _) = [("label", dotQuote x)]
getNodeLab s = [("label", dotQuote (seqLab s))]
getNodeLab _ = [emptyLab]
    
toGraph :: Deriv -> [String]    
toGraph d = w 
  where (_,_,w) = runRWS tg () 1
        tg = do
          tell ["digraph G {"
               ,"node[shape=circle]"
                --       ,"node[fixedsize=True,width=0.5]"
                --        ,"graph[start=2];" -- Seed
               ,"rankdir=LR;"
               ,"ranksep=0.1;" -- none of this seems
--               ,"minlen=0.1;"
                --      ,  "size=5;"
               ,"edge [arrowhead=\"vee\",dir=\"forward\",len=\"0.1\"];"]
          toGraphMain d
          tell ["}"]           
                     

dot2tex :: [String] -> ([String], [String])
dot2tex gv = unsafePerformIO $ do
  let c = intercalate " " ["dot2tex" 
                          ,"--prog=dot" 
                          ,"--format=tikz" 
--                          ,"--tikzedgelabels"
                          ,"--texmode=math"
                          ,"--codeonly"]
  putStrLn $ "running " ++ c
  (stdin',stdout',stderr',_procId) <- runInteractiveCommand c
  mapM_ (hPutStrLn stdin') gv
  hClose stdin'
  o <- hGetContents stdout'
  e <- hGetContents stderr'
  return (lines o, lines e)

couplingDiag :: Deriv -> TeX
couplingDiag d = do
  forM_ gv $ \l -> texLn $ "%" ++ l
  env' "tikzpicture" [">=latex","line join=bevel","auto","scale=1"] $ do
    forM_ o $ texLn 
  forM_ e $ \l -> texLn $ "%" ++ l
  where (o,e) = dot2tex gv
        gv = toGraph d 