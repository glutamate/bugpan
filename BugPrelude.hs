{-# LANGUAGE OverloadedStrings #-}

module BugPrelude where

import Expr 
import HaskSyntaxUntyped
import EvalM
import Numbers
 

prelude :: [Declare] 

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
 "later t es" =: ("emap" $> ("laterF" $> "t") $> "es")
 --"seconds" =: sig 1, --dummy
 --"dt" =: 1 --dummy
          ]++solvers

pair3e e1 e2 e3 = Pair (Pair e1 e2) e3

nstep t dt = roundNum (t/dt)

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
    

