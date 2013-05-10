
erlangTest: Compilation.pdf
	rm -f *.mps

mainTest: Mx.pdf
	rm -f *.mps

%-delayed.mp: %.mp
	rm -f $@
	mpost $<
	mpost $<

%.pdf: %.tex %-delayed.mp
	pdflatex $<
	pdflatex $<

%.mp %.tex: %.hs *.hs
	ghc --make $<
	./$*

%.hspp: %.hs
	ghc -E $<

simpleTest:
	ghc -E Simple.hs
	cat Simple.hspp
	ghc --make Simple.hs
	./Simple
