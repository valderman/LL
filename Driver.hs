module Driver (main) where

import Cake
import Cake.MarXup
import Paper

main = do
  outputTexMp name
  cake empty $ pdflatexMarxup name
  
name = "paper"
  
  