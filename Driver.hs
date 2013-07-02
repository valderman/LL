module Driver (main) where

import Cake
import Cake.MarXup
-- import Paper
import Popl
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import System.Directory (doesFileExist)
import Control.Monad.Trans (liftIO)

importExternalFile :: FilePath -> Act ()
importExternalFile what = do
  e <- liftIO $ doesFileExist what
  when e $ do liftIO $ hPutStrLn stderr $ "refreshing " ++ what
              system ["cp",what,"."]
              return ()

main :: IO ()
main = do
  outputTexMp name
  cake empty $ do 
    importExternalFile "../gitroot/bibtex/jp.bib"
    importExternalFile "../gitroot/latex/unicodedefs.tex"
    pdflatexMarxup name
  
name :: String  
name = "paper"
