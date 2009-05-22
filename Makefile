bnfc:
	cd BNFC && bnfc -m -haskell Bugpan.cf
	cd BNFC && happy -gca ParBugpan.y
	cd BNFC && alex -g LexBugpan.x
	cd BNFC && sed -i -e 's/module \(.*\)Bugpan where/module BNFC.\1Bugpan where/' *.hs
	cd BNFC && sed -i -e 's/module ErrM where/module BNFC.ErrM where/' *.hs

	cd BNFC && sed -i -e 's/import \(\w*\)Bugpan/import BNFC.\1Bugpan/' *.hs
	cd BNFC && sed -i -e 's/import ErrM/import BNFC.ErrM/' *.hs
	ghc --make BNFC/TestBugpan