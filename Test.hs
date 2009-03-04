module Main where

import Expr
import Eval
import EvalM
import Run
import Control.Monad
import Numbers

main = {-lookup "cross1" `fmap`-} run prelude testProg 0.01 10

testProg =
    [Let "secs" (Var "seconds"),
     Let "secsp1" $ (Var "smap") $> (Var "incr") $> (Var "seconds"),
     Let "secsp1d2" $ Sig (SigAt (time-0.2) (Var "secsp1")),
     Let "secsp1d1" $ SigDelay (Var "secsp1") 15,
     --Let "intsig" $ Var "integrate" $> (Sig 1),
     --Let "intStep" (Lam "new" . Lam "old" $ (Var "old") + (Var "new")*(Var "fixedDt")),
     --Let "accsig" (Var "accum" $> Var "secs"),
     --Let "integrate" (Lam "s" $ (Var "sscan" $> (Var "intStep") $> (0) $> Var "s")),
     --Let "add34" (Var "add" $> 3 $> 4),
     Let "syn1" (Var "alpha" $> 0.39 $> 1) ,
     Let "gsyn" (Var "smap" $> (Var "alpha" $> 0.39) $> (Var "seconds")),
     --Let "cross1" $ crossesUp (Var "seconds") 0.5,
     --Let "fact5" (Var "fact" $> 5),
     --Let "fib6" (Var "fib" $> 6),
     --Let "cross2" $ crossesUp (Var "secsp1d1") 1.5,
     --Let "fib5" (Var "fib" $> 5),
     --Let "iterIncr" $ (Var "iterate" $> Var "incr" $> 1.0),
     SinkConnect (Var "gsyn") "plot"]

{-test = teval (1+1.5) 

teval e = unEvalM emptyEvalS $ eval e
add = Lam "x" $ Lam "y" $ M2 Add (Var "x") (Var "y") 
myAdd = App (App add 1) 2

ta = teval myAdd

t1 n = unEvalM emptyEvalS $ extEnv ("x",5) . extEnv ("y",6) $ eval (Var n)


-}

tA = tstEval (Var "alpha" $> 0.39 $> 1)
tN = tstEval (negate 3)

test = testExprs [
        Var "alpha" $> 0.39 $> 1 #= (NumV $ 0.1029803506111),
        negate 3 #= (-3)

       ]


testExprs :: [(E,V)] -> IO ()
testExprs [] = return ()
testExprs ((e,v):rest) = case sfEvalM $ tstEval e of
                           Right v' -> when (v'/=v) $ putStrLn $ show e++"="++show v'++", expected "++show v
                           Left err -> putStrLn $ "error in eval "++show e++": "++err


convolve :: E -> E -> E
convolve = undefined

-- crosses :: Signal -> Value -> Signal
crosses :: E -> E -> E
crosses sig val = Event (If (SigVal sig .>=. val .&. SigVal (SigDelay sig 0) .<. val .|. --FIXME as below
                      SigVal sig .<. val .&. SigVal (SigDelay sig 0) .>=. val) 
                     (Cons (Pair (time) (Const Unit)) Nil)
                     (Nil))
crossesUp :: E -> E -> E
crossesUp sig val = Event (If (SigVal sig .>=. val .&. SigVal (SigDelay sig ((-2)*abs val)) .<. val) --something wrong
                       (Cons (Pair (time) (Const Unit)) Nil)
                       (Nil))


sigAt sig evt =   Var "map" $> Lam "tvp" (SigAt (Var "fst" $> Var "tvp") sig) $> evt
time = SigVal (Var "seconds")

--letEvt "foo" 

-- :set -fbreak-on-exception
decr = Var "decr"

prelude :: Env
prelude = [
 "map" #= LamV (\lf -> do f <- unLamV lf
                            --return (LamV (unListV >>= sequence. map f >>= return . ListV))
                          return . LamV $ \lst -> do ls <- unListV lst
                                                     vs <- sequence $ map f ls
                                                     return $ ListV vs),
 "seconds" #= SigV (NumV . NReal),                           
 "incr" #= (LamV $ \x-> return $ x+1),
 "decr" #= (LamV $ \x-> return $ x-1),
 "decr2" #= (LamV $ \x-> return $ x-2),
 "fst" #= (LamV $ \x -> fst `fmap` unPairV x ),
 "snd" #= (LamV $ \x -> snd `fmap` unPairV x),

 "alpha" #= ev (Lam "tau" . Lam "t" $ If (Var "t" .<. 0) 0 (tau*tau*t *exp (negate tau * t))),

 "add" #= ev (Lam "x" $ Lam "y" $ Var "x" + Var "y") ,
 "mul" #= ev (Lam "x" $ Lam "y" $ Var "x" * Var "y") ,
 "fact"#= ev (Lam "n" $ If (Var "n" .==. 1) 
                               1 
                               (Var "n" * (Var "fact" $> (Var "n" -1)))),
 "iterate" #= ev (Lam "f" $ Lam "s0" $ 
          LetE ["s" #= ( 
                        Sig (If (time .<=. 0.05) (Var "s0")
                                    (Var "f" $> SigVal (SigDelay (Var "s") (Var "s0"))))
                       )] (Var "s")),
 "smap" #= ev (Lam "f" . Lam "s" $ Sig (Var "f" $> (SigVal $ Var "s"))),

 "sscan" #= ev (Lam "f" . Lam "v0" . Lam "s" $
                    LetE ["sr"#= (Sig $ (Var "f") $> (SigVal (Var "s")) $> (SigVal $ SigDelay (Var "sr") (Var "v0")))
                        ] $ Var "sr"),
 {-"sscan'" #= ev (Lam "f" . Lam "v0" . Lam "s" $
                    LetE ["sr"#= (sig $ "f" ^$> sigVal "s" $> sigDelay "sr")
                         ] $ Var "sr"),-}
 "intStep'" #= ev (Lam "new" . Lam "otp" $ Pair (("fst" ^$> "otp") + (Var "new")*(time-("snd" ^$> "otp"))) (time)),
-- "intStep" #= ev (Lam "new" . Lam "old" $ (Var "old") + (Var "new")*(Var "fixedDt")),
 "integrate'" #= ev (Lam "s" $ (Var "smap" $> Var "fst") $> (Var "sscan" $> (Var "intStep") $> (Pair 0 0) $> Var "s")),
-- "integrate" #= ev (Lam "s" $ (Var "sscan" $> (Var "intStep") $> (0) $> Var "s")),
 "fib" #= ev (LetE ["f" #= (Lam "n" $ If (Var "n" .<. 3) 
                                              (1) 
                                              ((Var "f" $> (decr $> (Var "n")))+(Var "f" $> (Var "decr2" $> (Var "n")))))] (Var "f")),

 "accum" #= ev (Lam "s" $ Var "sscan" $> Var "add" $> 0 $> Var "s")
 --"constSig" #= ev ( Lam "v" $ Sig (Var "v"))
-- "integrate" #= ev (Lam "s" $ (Var "sscan") $>  
          ]
              where ev e = unEvalM $ eval (extsEnv prelude emptyEvalS) e  
                    tau = Var "tau"
                    t = Var "t"
    
preludeSt = extsEnv prelude emptyEvalS     

tstEval = eval preludeSt


infixl 1 #=                                         
x #= y = (x,y) 
