all:
	happy -gca ParBugpan.y
	alex -g LexBugpan.x
	latex DocBugpan.tex
	dvips DocBugpan.dvi -o DocBugpan.ps
	ghc --make TestBugpan.hs -o TestBugpan
clean:
	 rm -f *.log *.aux *.hi *.o *.dvi DocBugpan.ps *.o
distclean: clean
	 rm -f DocBugpan.* LexBugpan.* ParBugpan.* ParBugpanData.* LayoutBugpan.* SkelBugpan.* PrintBugpan.* TestBugpan.* AbsBugpan.* TestBugpan ErrM.* SharedString.* Bugpan.dtd XMLBugpan.* Makefile*
