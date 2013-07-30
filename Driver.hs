module Driver (main) where

import Cake hiding (system)
import Cake.MarXup
import MarXup.Tex
-- import Paper
import Popl
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import System.Directory (doesFileExist)
import Control.Monad.Trans (liftIO)
import System.Process (system)
import Data.List (intercalate)

importExternalFile :: FilePath -> Act ()
importExternalFile what = do
  e <- liftIO $ doesFileExist what
  when e $ liftIO $ do hPutStrLn stderr $ "refreshing " ++ what
                       system $ intercalate " " ["cp",what,"."]
                       return ()

svgRender :: IO ()
svgRender = do
  system "rm -f *.eps *.svg *.mps *-proof.tex *.mpx *.mp"
  outputTexMp True SVG name
  system "mpost paper.mp"
  system "pdflatex --shell-escape paper"
  return ()

main :: IO ()
main = do
  outputTexMp True EPS name
  cake empty $ do 
    importExternalFile "../gitroot/bibtex/jp.bib"
    importExternalFile "../gitroot/latex/unicodedefs.tex"
    pdflatexMarxup name

mainSansAppendix :: IO ()
mainSansAppendix = do
  outputTexMp False EPS name
  cake empty $ do 
    importExternalFile "../gitroot/bibtex/jp.bib"
    importExternalFile "../gitroot/latex/unicodedefs.tex"
    pdflatexMarxup name


name :: String  
name = "paper"
