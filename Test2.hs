{-# LANGUAGE OverloadedStrings #-}

module Main where

import Expr 
import Eval
import EvalM
--import Run
import Control.Monad
import Numbers
import Data.Char
import Traverse
import Control.Monad.State.Strict
--import Debug.Trace
import Transform
import Compiler
import ImpInterpret
import Stages
import Data.List
import Database
import HaskSyntaxUntyped
import UnitTesting hiding (evalsTo)

evalsTo = evalsToInContext (evalManyAtOnce $ declsToEnv prelude)

test_map_fst = [
                  "map" $> "incr" $> list [1,2,3] `evalsTo` ListV [2, 3, 4]
                 ,"fst" $> (Pair 1 2) `evalsTo` 1
                 ,"snd" $> (Pair 1 2) `evalsTo` 2]

test_sum =  "sum" $> list [1,2,3] `evalsTo` 6

type Program = [Declare]


prelude = [
 "smap f s" =: (sig $ "f" $> val "s"),
 "incr x" =: "x" + 1,
 "add x y" =: "x" + "y",
 "sscan f v0 s" =: LetE ["sr" =: (sig $ "f" $> (val "s") $> (val $ delay "sr" "v0"))
                  ] "sr",
 "integrate" =: "sscan" $> "intStep" $> 0,
 "intStep new old" =: ("old" + "new"*"dt"),
 "crosses val sig" =: (Event 
                       (If (val ("sig") .>=. ("val") .&. 
                            (val (delay ("sig") 0)) .<. ("val")) --not 0!
                        (Cons (Pair (val ("seconds")) (val ("sig"))) Nil) 
                        (Nil))),
 "eventIf p" =: (Event (If ("p") 
                                   (Cons (Pair (val ("seconds")) (Const Unit)) Nil) 
                                   (Nil))), 
 "round" =: (Const . LamV $ \(NumV n)->return . NumV $ roundNum n),
 "floor" =: (Const . LamV $ \(NumV n)->return . NumV $ floorNum n),
 "fraction x" =: ("x" - ("floor" $> "x")),
 "every ivl" =: ("eventIf" $> ("fraction" $> ((val $ "seconds")/"ivl") .<. "dt")),
 "map f lst" =: (Case "lst" [(PatNil, Nil),
                             (PatCons (PatVar "x") (PatVar "xs"), Cons ("f" $> "x") ("map" $> "f" $> "xs"))]),
 "sum lst" =: (Case "lst" [(PatNil, 0),
                           (PatCons (PatVar "x") (PatVar "xs"), "x"+("sum" $> "xs"))]),
 --"sum" =: (Const . LamV $ \vs -> sum `fmap` unListV vs),
 "fst pr" =: (Case "pr" [(PatPair (PatVar "x") (PatIgnore), "x")]),
 "snd pr" =: (Case "pr" [(PatPair (PatIgnore) (PatVar "x"), "x")]),
 "min x y" =: (If ("x" .>. "y") "y" "x"),
 "max x y" =: (If ("x" .<. "y") "y" "x"),
 "enow es" =: (("enowAux" $> (val $ "seconds") $> "dt" $> "es")),
 "enowAux" =: (Const . LamV $ \(NumV t) -> return $ LamV $ \(NumV dt) -> return $ LamV $ \(ListV es) -> do
                                      let dropF (PairV (NumV te) _) = nstep te dt > nstep t dt
                                      let takeF (PairV (NumV te) _) = nstep te dt == nstep t dt
                                      return $ ListV (takeWhile takeF $ dropWhile dropF es)),
 "emap f evs" =: (Event ("map" $> "f" $> ("enow" $> "evs"))),
 "convolve s es" =: (sig ("sum" $> ("map" $> 
    (Lam "e" (SigAt (val "seconds" - ("fst" $> "e" )) "s")) $> "es"))),
 "laterF t e" =: (Pair (("fst" $> "e")+"t") ("snd" $> "e")),
 "later t es" =: ("emap" $> ("laterF" $> "t") $> "es"),
 "seconds" =: sig 1, --dummy
 "dt" =: 1 --dummy
          ]++solvers

pair3e e1 e2 e3 = Pair (Pair e1 e2) e3

nstep t dt = roundNum (t/dt)

testProg  = [
 "seconds" *> "print",
 "alpha tau t " =: (If ("t" .<. 0) 0 ("tau"*"tau"*"t" * (exp . negate $ ("tau" * "t")))),
 "rndSpikeSig" <* "bernoulli 100",
 "rndSpike" =: ("eventIf" $> val ("rndSpikeSig")),
 "preSpike" =: ("every" $> 0.01),
 "gsyn" =: ("smap" $> ("alpha" $> 300) $> ("seconds")),
 "gsyn" *> "print",
 --"fr" =: (Sig (("fraction" $> (val ("seconds"))/0.01))),
 --SinkConnect ("fr") "print",
 Stage "gsyn" (-1),
 "gcell" *> "print",
 "gcell" =: ("convolve" $> "gsyn" $> "rndSpike"),
 "cellOde v" =: (Sig $ ((val $ "gcell")*(0.3e-12)-(("v"+0.07)/1e9))/(2.5e-12)),
 --"vm" =: ("solveOde" $> "cellOde" $> (-0.07)),
 --"vm" =: ("solveOde" $> ((Lam "v" $ Sig $ ((val $ "gcell")*(1e-12)-(("v"+0.07)/1e9))/(2.5e-12))) $> (-0.07)),
 --"vm" *> "print",
 {-"intfire" =: (LetE [("spike", "crosses" $> (-0.04) $> "vm"),
                     ("vm", Switch [("spike", Lam "tsp" . Lam "_" $ ("solveOdeFrom" $> "tsp" $> "cellOde" $> (-0.07)))
                                     ] $ ("solveOde" $> "cellOde" $> (-0.07))  )
                     ] ("vm")), -}

 "vm" =: (Switch ["spike" ~> (lam "_ _" $ sig (-0.07)),
                  "refrac_end" ~> (lam "tsp _" $ ("solveOdeFrom" $> ("tsp"+"dt") $> "cellOde" $> (-0.07)))
                 ] ("solveOde" $> "cellOde" $> (-0.07))),
 "spike" =: ("crosses" $> (-0.04) $> "vm"),
 "refrac_end" =: ("later" $> 0.002 $> "spike"),
 
 "vm" *> "store",
 "spike" *> "store"]

loomProg = [
 --looming
 "lov" =: 0.04,
 "l" =: 0.298,
 "v" =: ("l"/("lov"*2)),
 "centreCube l" =: (Translate (pair3e (-"l"/2) (-"l"/2) 0) $ Box (pair3e "l" "l" "l")),

 "distance" =: sig ("min" $> ("v"*(val "seconds"-5)) $> (-0.17)),
 "black" =: Pair (Pair 0 0) 0,
 "green" =: Pair (Pair 0 1) 0,
 "loomObj" =: sig (Colour "black" $ Translate (Pair (Pair 0 0) (val "distance")) ("centreCube" $> "l"))
 ,"loomObj" *> "screen"
 --,"seconds" *> "print"
 --,"loomObj" *> "print"
  ]

solvers =  [
 "iterate f s0" =:
          LetE ["s" =:  Sig (If (val "seconds" .<=. ("dt"/2)) 
                                    ("s0")
                                    ("f" $> val (delay "s" "s0")))
                       ] ("s"),
 "iterateFrom t0 f s0" =: 
          LetE ["s" =: Sig (If (val "seconds" .<=. ("t0"+("dt"/2))) 
                                    ( "s0")
                                    ("f" $> val (delay "s" "s0"))
                       )] "s",
 "solveStep sf v0 old" =: ("old" + ( val (delay ( "sf" $>  "old") ( "v0"))) * "dt"),
 "solveOde sf v0" =: ( "iterate" $> ("solveStep" $> "sf" $> "v0") $> "v0"),
 "solveOdeFrom t0 sf v0" =: ("iterateFrom" $> "t0" $> ("solveStep" $> "sf" $> "v0") $> "v0")]
    


infixl 1 #=                                         
x #= y = (x,y) 


ppProg = mapM (putStrLn . ppDecl) 

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst . runTM $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

runTM = runTravM loomProg (declsToEnv prelude)

--infixl 1 =:                                         
--x =: y = Let x y 

testTransform :: TravM () -> [Declare] -> [Declare]
testTransform tr ds = snd . runTravM ds (declsToEnv prelude) $ tr

takeMoreAndMore :: [a] -> [[a]]
takeMoreAndMore xs = map (`take` xs) [1..length xs] 


allTransforms :: IO ()
allTransforms = do
  putStrLn "\nprelude"
  ppProg prelude
  putStrLn "\ninitial"
  ppProg testProg
  forM_ (takeMoreAndMore transforms) $ \trnsfs -> do 
      putStrLn $ "\nafter: "++(intercalate ", " $ map snd trnsfs)
      ppProg (snd . runTM $ sequence_ (map fst trnsfs))
  return ()


prelEnv = declsToEnv prelude

main = do putStrLn "\ninitial"
          --ppProg prelude
          --ppProg testProg
          putStrLn "\ntransformed"
          let prg = snd . runTM $ transform
          let complPrel =  fst . runTM $ compilablePrelude
          --ppProg (prg)
          putStrLn "\ncompiled"
          let stmts = compile (complPrel++prg)
          mapM_ (putStrLn . ppStmt) $ stmts
          putStrLn "\nrunning"
          execInStages (complPrel++prg) 0.005 5
          waitSecs 1
        
test1 = runNtimes 3 0.001 0.03 0.1 testProg prelEnv 
  --let compPrel = evalManyAtOnce prelEnv
  --let sess = emptySession {sessPrelude = prelEnv}
  --runOnce 0.001 0 0.03 testProg sess 

--process :: E-> TravM Process

-- :set -fbreak-on-exception 

 --"aval" =: 5,
 --"secsp1d1" =: ((Var "smap") $> (Var "incr") $> (delay (Var "seconds") (0))),
 --"accum_secs_plus1" =: ((Var "sscan") $> (Var "add") $> 0 $> ((Var "smap") $> (Var "incr") $> (Var "seconds"))  ),
 --"accsecs" =: ((Var "sscan") $> (Var "add") $> 0 $> (Var "seconds")  ),
 --"intsecs" =: ((Var "integrate" $> (Var "accum_secs_plus1"))),
 --"overp5" =: (Var "crosses" $> 0.02 $> Var "seconds"),
 --"over_intsecs" =: (Var "crosses" $> (val(Var "intsecs")) $> Var "seconds"),
 --SinkConnect (Var "intsecs") "print",
 --"swsig" =: (Switch [(Var "overp5", Lam "x" $ Sig (Var "x"))] (Sig 1)),
 --SinkConnect (Var "swsig") "print",
 --"myOde" =: (Var "solveOde" $> (Lam "y" $ Sig (0-Var "y")) $> 1),
