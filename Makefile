
mainTest: Mx.pdf
	rm -f *.mps

%-delayed.mp: %.mp
	rm -f $@
	mpost $<
	mpost $<

%.pdf: %.tex %-delayed.mp 
	pdflatex $<
	pdflatex $<

%.mp %.tex: %.hs
	ghc --make $<
	./$*

%.hspp: %.hs *.hs
	ghc -E $<

simpleTest:
	ghc -E Simple.hs
	cat Simple.hspp
	ghc --make Simple.hs
	./Simple
