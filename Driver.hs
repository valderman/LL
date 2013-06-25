module Driver (main) where

import Cake
import Cake.MarXup
import Popl

main :: IO ()
main = do
  outputTexMp name
  cake empty $ pdflatexMarxup name
  
name :: String  
name = "paper"
