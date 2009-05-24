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
import BugPrelude

evalsTo = evalsToInContext (evalManyAtOnce $ declsToEnv prelude)

test_map_fst = [
                  "map" $> "incr" $> list [1,2,3] `evalsTo` ListV [2, 3, 4]
                 ,"fst" $> (Pair 1 2) `evalsTo` 1
                 ,"snd" $> (Pair 1 2) `evalsTo` 2]

test_sum =  "sum" $> list [1,2,3] `evalsTo` 6

type Program = [Declare]


testProg  = [
 --"seconds" *> "print",
 "alpha tau t " =: (If ("t" .<. 0) 0 ("tau"*"tau"*"t" * (exp . negate $ ("tau" * "t")))),
 "rndSpikeSig" <* "bernoulli 100",
 "rndSpike" =: ("eventIf" $> val ("rndSpikeSig")),
 "preSpike" =: ("every" $> 0.01),
 "gsyn" =: ("smap" $> ("alpha" $> 300) $> ("seconds")),
 -- "gsyn" *> "print",
 --"fr" =: (Sig (("fraction" $> (val ("seconds"))/0.01))),
 --SinkConnect ("fr") "print",
 Stage "gsyn" (-1),
 -- "gcell" *> "print",
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


infixl 1 #=                                         
x #= y = (x,y) 


ppProg = mapM (putStrLn . ppDecl) 

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst . runTM $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

runTM = runTravM (loomProg++ prelude) []

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


--prelEnv = declsToEnv prelude

main = testQ

testQ = do test1
           s <- lastSession "/home/tomn/sessions/"
           print s
           qres <- runAskM s $ signals "vm"
           --plot qres
                         
           --mapM print qres
           return ()

test = do putStrLn "\ninitial"
          --ppProg prelude
          --ppProg testProg
          --putStrLn "\ntransformed"
          let prg = snd . runTM $ transform 
          
          --ppProg (prg)
          --putStrLn "\ncompiled"
          let stmts = compile (prg)
          --mapM_ (putStrLn . ppStmt) $ stmts
          putStrLn "\nrunning"
          execInStages (prg) 0.01 5 return
          waitSecs 1
         
test1 = do sess <- newSession "/home/tomn/sessions/"
           runNtimes 3 0.001 0.03 0 0.1 (prelude++testProg) sess 
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
