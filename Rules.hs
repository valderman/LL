module Rules where

import LL
import Symheap
import Pretty

rule = Deriv [] [("",meta "Γ"),("z",meta "A" :&: meta "B")] (With True 1 (What "a"))




