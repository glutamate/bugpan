bnfc: BNFC/Bugpan.cf
	rm -f BNFC/*.hs
	cd BNFC && bnfc -m -haskell Bugpan.cf
	cd BNFC && happy -gca ParBugpan.y
	cd BNFC && alex -g LexBugpan.x
	cd BNFC && sed -i -e 's/module \(.*\)Bugpan where/module BNFC.\1Bugpan where/' *.hs
	cd BNFC && sed -i -e 's/module ErrM where/module BNFC.ErrM where/' *.hs

	cd BNFC && sed -i -e 's/import \(\w*\)Bugpan/import BNFC.\1Bugpan/' *.hs
	cd BNFC && sed -i -e 's/import ErrM/import BNFC.ErrM/' *.hs
	cd BNFC && rm *.x
	cd BNFC && rm *.y



runbugpan: 
	ghc --make RunBugpan -O2 -threaded -lcomedi Comedi/comedi_hs_helper.o


prof:
	ghc -prof -auto --make RunBugpan -lcomedi Comedi/comedi_hs_helper.o

nodaq: 
	ghc --make -O2 RunBugpan -threaded

driver:
	ghc --make Driver -threaded -lcomedi Comedi/comedi_hs_helper.o

runloom:
	ghc --make RunLoom 

testacq:
	ghc --make Tests/TestAcqOnly -lcomedi Comedi/comedi_hs_helper.o
#	sudo time ./TestAcqOnly +RTS -p

testgainopt:
	rm -f ValueIO.o Query.o QueryUtils.o QueryTypes.o
	ghc --make TestGain -O2 -lcomedi Comedi/comedi_hs_helper.o

tests:	testgain iotest testacq
	ghc UnitTesting.hs -e 'runAllTests'


testgain:
	ghc --make Tests/TestGain -lcomedi Comedi/comedi_hs_helper.o -i..

iotest:
	ghc --make Tests/IOTest -lcomedi Comedi/comedi_hs_helper.o -i..


bugsess:
	ghc --make -O2	 BugSess -lcomedi Comedi/comedi_hs_helper.o

bugsessprof:
	ghc --make BugSess -prof -auto-all -lcomedi Comedi/comedi_hs_helper.o

testgainprof:
	ghc --make TestGain -prof -auto-all -lcomedi Comedi/comedi_hs_helper.o

comedi_helper: 
	gcc -c -g -o Comedi/comedi_hs_helper.o Comedi/comedi_hs_helper.c

clean: 
	rm *.o *.hi
	rm Comedi/*.o

poissonprof:
	ghc --make -O2 -prof -auto-all PoissonSpikesModel.hs -fexcess-precision
poissonnoprof:
	gcc -c -g -o poisson.o poisson.c
	ghc --make -O2 PoissonSpikesModel.hs -threaded -fexcess-precision poisson.o