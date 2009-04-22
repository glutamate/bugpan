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
import Debug.Trace
import Transform

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
                                                         (Cons (Pair (SigVal (Var "seconds")) (Const Unit)) Nil) 
                                                         (Nil))),
            "seconds" =: Sig 1, --dummy
            "dt" =: 1 --dummy
          ]

testProg  = [--"secsp1" =: ((Var "smap") $> (Var "incr") $> (Var "seconds")),
             "aval" =: 5,
             --"secsp1d1" =: ((Var "smap") $> (Var "incr") $> (SigDelay (Var "seconds") (0))),
             "accum_secs_plus1" =: ((Var "sscan") $> (Var "add") $> 0 $> ((Var "smap") $> (Var "incr") $> (Var "seconds"))  ),
             --"accsecs" =: ((Var "sscan") $> (Var "add") $> 0 $> (Var "seconds")  ),
             "intsecs" =: ((Var "integrate" $> (Var "accum_secs_plus1"))),
             "over5" =: (Var "crosses" $> 5 $> Var "seconds"),
             "over_intsecs" =: (Var "crosses" $> (SigVal(Var "intsecs")) $> Var "seconds")

            ]

ppProg prg = forM_ prg $ \e -> case e of 
                                 Let n e-> putStrLn (n++" = " ++ pp e)
                                 

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst . runTM $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

runTM = runTravM testProg (declsToEnv prelude)

infixl 1 =:                                         
x =: y = Let x y 


test = do putStrLn "prelude"
          ppProg prelude
          putStrLn "\ninitial"
          ppProg testProg
          putStrLn "\ntransformed"
          ppProg (snd . runTM $ do whileChanges substHasSigs  
                                   betaRedHasSigs 
                                   letFloating 
                                   sigFloating 
                                   unDelays
                                   explicitCopying
                 )

          --return $ hasSigProg testProg

--process :: E-> TravM Process

