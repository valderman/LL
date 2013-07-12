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


fermion s = node [("shape","circle"),getNodeLab s]
boson s = node [("shape","none"),getNodeLab s]
hypothesis = node [("shape","circle"),("color","white"),emptyLab] 
             -- for some reason this is more compact than "shape=none"

specialEdge = edgeAutoDir [("style","dotted")]
edge = edgeAutoDir []

edgeAutoDir, edgeFixedDirection :: [Attr] -> NodeRef -> NodeRef -> Type -> GGen ()

edgeAutoDir attrs sn tn ty@(Meta True "Ξ" []) = edgeFixedDirection attrs tn sn (neg ty)
edgeAutoDir attrs sn tn ty@(Meta False _  _ ) = edgeFixedDirection attrs tn sn (neg ty)
edgeAutoDir attrs sn tn ty = edgeFixedDirection attrs sn tn ty

edgeFixedDirection attrs sn tn ty = do
  comment $ "  edge of type " ++ P.render (pClosedType ty)
  tell [sn ++ " -> " ++ tn ++ mkAttrs (("label",typ):attrs) ++ ";"]
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

toGraphPart :: [Name] -> [(Name,Type)] -> Seq -> GGen [NodeRef]
toGraphPart te e (Cut v v' ty γ s t) = do
  let (e0,e1) = splitAt γ e
  (s':sn) <- toGraphPart te ((v,neg ty):e0) s
  (t':tn) <- toGraphPart te ((v', ty)  :e1) t
  edge s' t' ty
  return $ sn ++ tn
toGraphPart te e (Exchange π s) = do
  comment $ "  exchange"
  applyPerm π <$> toGraphPart te (applyPerm π e) s  
toGraphPart te e this@(With True _ v b x s) = do  
  let (v0,(_,~(vt :&: vt')):v1) = splitAt x e
      wt = if b then vt else vt'
  (n0,n:n1) <- splitAt x <$> toGraphPart te (v0++(v,wt):v1) s
  thisNode <- boson this
  specialEdge thisNode n wt
  return (n0++thisNode:n1)
toGraphPart te e this@(SOne True x s) = do  
  let (v0,(_,~One):v1) = splitAt x e
  (n0,n1) <- splitAt x <$> toGraphPart te (v0++v1) s
  thisNode <- boson this
  return (n0++thisNode:n1)
toGraphPart te e this@(TApp True _ w x tyB s) = do  
  let (v0,(_,~(Forall _ tyA)):v1) = splitAt x e
      ty = subst0 tyB ∙ tyA     
  (n0,n:n1) <- splitAt x <$> toGraphPart te (v0++(w,ty):v1) s
  thisNode <- boson this
  specialEdge thisNode n ty
  return (n0++thisNode:n1)
toGraphPart te e this@(Offer True v x s) = do  
  let (v0,(_,~(Quest tyA)):v1) = splitAt x e
  (n0,n:n1) <- splitAt x <$> toGraphPart te (v0++(v,tyA):v1) s
  thisNode <- boson this
  specialEdge thisNode n tyA
  return (n0++thisNode:n1)
toGraphPart te e this@(Mem ty x n t u) = do  
   let (t',u') = splitAt x e
   (tn:tns) <- toGraphPart te (("_x",neg ty):t') t
   (un,uns) <- splitAt n <$> toGraphPart te (replicate n ("x",Bang ty) ++ u') u
   thisNode <- boson this
   specialEdge thisNode tn ty
   forM un $ \un' -> 
     specialEdge thisNode un' (Bang ty)
   return (tns++uns)  
toGraphPart te e this@(Alias True x w' s) = do  
  let (v0,(w,~(Bang tyA)):v1) = splitAt x e 
  (n:n1,n':n2) <- splitAt (x+1) <$> toGraphPart te ((w,Bang tyA):v0++(w',Bang tyA):v1) s
  thisNode <- boson this
  specialEdge thisNode n (Bang tyA)
  specialEdge thisNode n' (Bang tyA)
  return (n1++thisNode:n2)
toGraphPart te e this@(Par True _ v v' x s t) = do
  let (v0,(_w,(vt :|: vt')):v1) = splitAt x e
  sn0 <- toGraphPart te (v0++[(v,vt)]) s
  let sn = init sn0
      s' = last sn
  (t':tn) <- toGraphPart te ((v',vt'):v1) t
  thisNode <- boson this
  specialEdge thisNode s' vt
  specialEdge thisNode t' vt'
  return (sn++thisNode:tn)
toGraphPart te e s@(Cross True _ v v' x t) = do
  let (v0,(_w,(vt :⊗: vt')):v1) = splitAt x e
  tn <- toGraphPart te (v0++(v,vt):(v',vt'):v1) t 
  let (ts0,(t1:t2:ts3)) = splitAt x tn
  thisNode <- boson s
  specialEdge thisNode t1 vt
  specialEdge thisNode t2 vt'
  return (ts0++thisNode:ts3)
toGraphPart _te e s = do 
  comment $ "  fermion"
  thisNode <- fermion s
  return $ replicate (length e) thisNode
  
  
toGraphMain :: Deriv -> GGen ()
toGraphMain (Deriv te e s) = do
  hs <- toGraphPart  te e s
  forM_ (zip hs e) $ \(n,(_,ty)) -> do
    hypNode <- hypothesis
    edge hypNode n ty
    
getNodeLab :: Seq -> Attr
getNodeLab (What x _) = ("label", dotQuote x)
getNodeLab s = ("label", dotQuote (seqLab s))
getNodeLab _ = emptyLab
    
toGraph :: Deriv -> [String]    
toGraph d = w 
  where (_,_,w) = runRWS tg () 1
        tg = do
          tell ["digraph G {"
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
  env' "tikzpicture" [">=latex","line join=bevel","auto","scale=0.6"] $ do
    cmd0 "small"
    forM_ o $ texLn 
  forM_ e $ \l -> texLn $ "%" ++ l
  where (o,e) = dot2tex gv
        gv = toGraph d 