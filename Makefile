bnfc: BNFC/Bugpan.cf
	rm -f BNFC/*.hs
	cd BNFC && bnfc -m -haskell Bugpan.cf
	cd BNFC && happy -gca ParBugpan.y
	cd BNFC && alex -g LexBugpan.x
	cd BNFC && sed -i -e 's/module \(.*\)Bugpan where/module BNFC.\1Bugpan where/' *.hs
	cd BNFC && sed -i -e 's/module ErrM where/module BNFC.ErrM where/' *.hs

	cd BNFC && sed -i -e 's/import \(\w*\)Bugpan/import BNFC.\1Bugpan/' *.hs
	cd BNFC && sed -i -e 's/import ErrM/import BNFC.ErrM/' *.hs


runbugpan: 
	ghc --make RunBugpan -O2 -threaded -lcomedi Comedi/comedi_hs_helper.o


prof:
	ghc -prof -auto --make RunBugpan -lcomedi Comedi/comedi_hs_helper.o

nodaq: 
	ghc --make -O2 RunBugpan -threaded

driver:
	ghc --make Driver -threaded -lcomedi Comedi/comedi_hs_helper.o

runloom:
	ghc --make RunLoom -lcomedi Comedi/comedi_hs_helper.o

testacq:
	ghc --make TestAcqOnly -prof -auto-all -lcomedi Comedi/comedi_hs_helper.o
	sudo time ./TestAcqOnly +RTS -p

testgain:
	ghc --make TestGain -lcomedi Comedi/comedi_hs_helper.o

bugsess:
	ghc --make BugSess -lcomedi Comedi/comedi_hs_helper.o

testgainprof:
	ghc --make TestGain -prof -auto-all -lcomedi Comedi/comedi_hs_helper.o

comedi_helper: 
	gcc -c -g -o Comedi/comedi_hs_helper.o Comedi/comedi_hs_helper.c

tests:
	ghc UnitTesting.hs -e 'runAllTests'

clean: 
	rm *.o *.hi
	rm Comedi/*.o
