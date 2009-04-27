module Test2 where

import Expr
import Eval
import EvalM
import Run
import Control.Monad
import Numbers
import Data.Char
import Traverse
import Control.Monad.State.Strict
--import Debug.Trace
import Transform
import Compiler
import ImpInterpret

type Program = [Declare] 

prelude = [ "smap" =: (Lam "f" . Lam "s" $ Sig (Var "f" $> (SigVal $ Var "s"))),
            "incr" =: (Lam "x" (Var "x" + 1)),
            "add" =: (Lam "x" $ Lam "y" $ Var "x" + Var "y"),
            "sscan" =: (Lam "f" . Lam "v0" . Lam "s" $
                        LetE [("sr", (Sig $ (Var "f") $> (SigVal (Var "s")) $> (SigVal $ SigDelay (Var "sr") (Var "v0"))))
                             ] $ Var "sr"),
            "integrate" =: ((Var "sscan" $> (Var "intStep") $> (0))),
            "intStep" =: (Lam "new" . Lam "old" $ (Var "old") + (Var "new")*(Var "dt")),
            "crosses" =: (Lam "val" . Lam "sig" $ Event (If 
                                                         (SigVal (Var "sig") .>=. (Var "val") .&. 
                                                          (SigVal (SigDelay (Var "sig") 0)) .<. (Var "val")) --not 0!
                                                         (Cons (Pair (SigVal (Var "seconds")) (SigVal (Var "sig"))) Nil) 
                                                         (Nil))),
            "alpha" =: let tau = Var "tau" in (Lam "tau" . Lam "t" $ If (Var "t" .<. 0) 0 (tau*tau*t *exp (negate tau * t))),
            "seconds" =: Sig 1, --dummy
            "dt" =: 1 --dummy
          ]++solvers

testProg  = [--"secsp1" =: ((Var "smap") $> (Var "incr") $> (Var "seconds")),
             "aval" =: 5,
             --"secsp1d1" =: ((Var "smap") $> (Var "incr") $> (SigDelay (Var "seconds") (0))),
             "accum_secs_plus1" =: ((Var "sscan") $> (Var "add") $> 0 $> ((Var "smap") $> (Var "incr") $> (Var "seconds"))  ),
             --"accsecs" =: ((Var "sscan") $> (Var "add") $> 0 $> (Var "seconds")  ),
             "intsecs" =: ((Var "integrate" $> (Var "accum_secs_plus1"))),
             "overp5" =: (Var "crosses" $> 0.5 $> Var "seconds"),
             "over_intsecs" =: (Var "crosses" $> (SigVal(Var "intsecs")) $> Var "seconds"),
             SinkConnect (Var "intsecs") "print",
             "swsig" =: (Switch [(Var "overp5", Lam "x" $ Sig (Var "x"))] (Sig 1)),
             SinkConnect (Var "swsig") "print",
             "myOde" =: (Var "solveOde" $> (Lam "y" $ Sig (0-Var "y")) $> 1)

           {-"intfire" =: (LetE [("spike", Var "crosses" $> -0.04 $> Var "vm"),
                                 ("vm", 
                                ] (Var "vm")) -}
            ]

solvers =  [
 "iterate" =: (Lam "f" $ Lam "s0" $ 
          LetE ["s" #= ( 
                        Sig (If (time .<=. (dt/2)) 
                                    (Var "s0")
                                    (Var "f" $> SigVal (SigDelay (Var "s") (Var "s0"))))
                       )] (Var "s")),

 "solveStep" =: (Lam "sf" . Lam "v0" . Lam "old" $ (Var "old") + ( SigVal (SigDelay (Var "sf" $> Var "old") (Var "v0"))) * Var "dt"),
 "solveOde" =: (Lam "sf" . Lam "v0" $ Var "iterate" $> (Var "solveStep" $> Var "sf" $> Var "v0") $> Var "v0")]
    where x #= y = (x,y) 
          dt = (Var "dt")
          time = Var "seconds"



infixl 1 #=                                         
x #= y = (x,y) 


ppProg = mapM (putStrLn . ppDecl) 

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst . runTM $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

runTM = runTravM testProg (declsToEnv prelude)

infixl 1 =:                                         
x =: y = Let x y 


test = do putStrLn "\ninitial"
          ppProg prelude
          ppProg testProg
          putStrLn "\ntransformed"
          let prg = snd . runTM $ transform
          let complPrel =  fst . runTM $ compilablePrelude
          ppProg (prg)
          putStrLn "\ncompiled"
          let stmts = compile (complPrel++prg)
          mapM_ (putStrLn . ppStmt) $ stmts
          putStrLn "\nrunning"
          exec stmts 0.1 1

          --return $ hasSigProg testProg

--process :: E-> TravM Process

-- :set -fbreak-on-exception