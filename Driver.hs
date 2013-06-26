module Driver (main) where

import Cake
import Cake.MarXup
import Popl
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import System.Directory (doesFileExist)
import Control.Monad.Trans (liftIO)

refresh_jp_bib :: Act ()
refresh_jp_bib = do
  let jpbib = "../gitroot/bibtex/jp.bib"
  e <- liftIO $ doesFileExist jpbib
  when e $ do liftIO $ hPutStrLn stderr "refreshing bib"
              system ["cp",jpbib,"."]
              return ()

main :: IO ()
main = do
  outputTexMp name
  cake empty $ do 
    refresh_jp_bib
    pdflatexMarxup name
  
name :: String  
name = "paper"
