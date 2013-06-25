module GraphViz where
    
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


type GGen a = RWS () [String] Int a
type NodeRef = String
type Port = Int

node :: String -> GGen NodeRef
node extraAttrs'  = do
  let extraAttrs = extraAttrs' ++ [','|not $ null extraAttrs']
  r <- get
  put (r+1)
  let ref = "nd" ++ show r 
  tell [ref ++ "[" ++ extraAttrs ++ "width=5,label=\"\"];"]
  return ref
  
edge :: NodeRef -> NodeRef -> Type -> GGen ()
edge sn tn ty@(Meta True "Ξ" []) = edge tn sn (neg ty)
edge sn tn ty = do
  comment $ "  edge of type " ++ P.render (pClosedType ty)
  tell [sn ++ " -> " ++ tn ++ "[label=" ++ typ ++ "];"]
  -- len=\"0.5\", : Does not really work; the edge label is placed as if len=1
  where typ = show $ concat $ render $ math $ texClosedType ty
  
rm :: Maybe Int -> [t] -> [t]
rm Nothing xs = xs        
rm (Just x) xs = l++r where (l,_:r) = splitAt x xs

comment :: String -> GGen ()
comment x = tell $ ["// " ++ x]

data HInfo = External Type | Internal NodeRef Type | Done

toGraphPart :: Maybe Port -> [Name] -> [(Name,HInfo)] -> Seq -> GGen NodeRef
toGraphPart parent te e (Cut v v' ty γ s t) = do
    comment $ "  cut"
    sn <- toGraphPart (Just 0) te ((v,Done):e0) s
    tn <- toGraphPart (Just 0) te ((v',Internal sn ty)   :e1) t
    return $ if maybe True (< γ) parent then sn else tn
  where (e0,e1) = splitAt γ e
toGraphPart parent te e (Exchange π s) = do
  comment $ "  exchange"
  toGraphPart ((`indexOf` π) `fmap` parent) te [e !! i | i <- π] s
toGraphPart _parent _te e _ = do
  comment $ "  leaf"
  thisNode <- node "" -- "circle"
  forM_ e $ \ (_name,h) -> do
    case h of
      Done -> return ()
      External ty -> do 
        -- hypNode <- node $ "shape = " ++ show "none"
        hypNode <- node "color=white" -- $ "shape = " ++ show "none"
        edge hypNode thisNode ty
      Internal n ty -> edge n thisNode ty
  return thisNode    
    
indexOf :: Eq a => a -> [a] -> Int
indexOf _x [] = error "indexOf: not found"
indexOf x (y:ys) | x == y = 0
                 | otherwise = 1 + indexOf x ys

toGraphMain :: Deriv -> GGen ()
toGraphMain (Deriv te e s) = do
  tell ["digraph G {"
       ,"node[shape=circle]"
        --       ,"node[fixedsize=True,width=0.5]"
--        ,"graph[start=2];" -- Seed
       ,"rankdir=LR;"
       ,"ranksep=0;"
        --      ,  "size=5;"
       ,"edge [arrowhead=\"vee\",dir=\"forward\"];"]
  toGraphPart Nothing te [(nm,External ty) | (nm,ty) <- e] s
  tell ["}"]

toGraph :: Deriv -> [String]    
toGraph d = w where (_,_,w) = runRWS (toGraphMain d) () 1

dot2tex :: [String] -> ([String], [String])
dot2tex gv = unsafePerformIO $ do
  let c = intercalate " " ["dot2tex" 
                          ,"--prog=dot" 
                          ,"--format=tikz" 
                          ,"--tikzedgelabels"
                          ,"--texmode=raw"
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
  env' "tikzpicture" [">=latex","line join=bevel","auto","scale=0.08"] $ do
    forM_ o $ texLn 
  forM_ e $ \l -> texLn $ "%" ++ l
  where (o,e) = dot2tex gv
        gv = toGraph d 