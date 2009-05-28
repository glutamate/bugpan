bnfc: BNFC/Bugpan.cf
	rm -f BNFC/*.hs
	cd BNFC && bnfc -m -haskell Bugpan.cf
	cd BNFC && happy -gca ParBugpan.y
	cd BNFC && alex -g LexBugpan.x
	cd BNFC && sed -i -e 's/module \(.*\)Bugpan where/module BNFC.\1Bugpan where/' *.hs
	cd BNFC && sed -i -e 's/module ErrM where/module BNFC.ErrM where/' *.hs

	cd BNFC && sed -i -e 's/import \(\w*\)Bugpan/import BNFC.\1Bugpan/' *.hs
	cd BNFC && sed -i -e 's/import ErrM/import BNFC.ErrM/' *.hs
	ghc --make BNFC/TestBugpan

runbugpan: 
	ghc --make RunBugpan -threaded -lcomedi Comedi/comedi_hs_helper.o

nodaq: 
	ghc --make RunBugpan -threaded

comedi_helper: 
	gcc -c -g -o Comedi/comedi_hs_helper.o Comedi/comedi_hs_helper.c