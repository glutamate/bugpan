module Main where

import Expr
import Eval
import EvalM
import Run
import Numbers

main = {-lookup "cross1" `fmap`-} run prelude testProg 0.1 1

smapE :: E
smapE = Lam "f" $ Lam "sig" $ Sig (App (Var "f") (SigVal (Var "sig")))

smap f s = (App (App smapE f) s)

testProg =
    [Let "secs" (Var "seconds"),
     Let "secsp1" $ smap (Var "incr") (Var "seconds"),
     Let "secsp1d2" $ Sig (SigAt (time-0.2) (Var "secsp1")),
     Let "secsp1d1" $ SigDelay (Var "secsp1"),
     Let "cross1" $ crossesUp (Var "seconds") 0.5,
     Let "fact" fact,
     Let "fact5" (Var "fact" $> 5),
     Let "fib6" (Var "fib" $> 6),
     Let "cross2" $ crossesUp (Var "secsp1d1") 1.5,
     Let "fib5" (Var "fib" $> 5),
     Let "iterate" 
         (Lam "f" $ Lam "s0" $ 
          LetE ["s" #= ( 
                        Sig (If (time .<=. 0.05) (Var "s0")
                                    (Var "f" $> SigVal (SigDelay (Var "s"))))
                       )] (Var "s")),
     Let "iterIncr" $ (Var "iterate" $> Var "incr" $> 1.0),
--     Let "integrate" (Lam "s" (Sig ))
     Let "fib" $ LetE ["f" #= (Lam "n" $ If (Var "n" .<. 3) 
                                              (1) 
                                              ((Var "f" $> (decr $> (Var "n")))+(Var "f" $> (Var "decr2" $> (Var "n")))))] (Var "f"),
     SinkConnect (Var "iterIncr") "print"]

{-test = teval (1+1.5) 

teval e = unEvalM emptyEvalS $ eval e
add = Lam "x" $ Lam "y" $ M2 Add (Var "x") (Var "y") 
myAdd = App (App add 1) 2

ta = teval myAdd

t1 n = unEvalM emptyEvalS $ extEnv ("x",5) . extEnv ("y",6) $ eval (Var n)

-}

convolve :: E -> E -> E
convolve = undefined

-- crosses :: Signal -> Value -> Signal
crosses :: E -> E -> E
crosses sig val = Event (If (SigVal sig .>=. val .&. SigVal (SigDelay sig) .<. val .|. 
                      SigVal sig .<. val .&. SigVal (SigDelay sig) .>=. val) 
                     (Cons (Pair (time) (Const Unit)) Nil)
                     (Nil))
crossesUp :: E -> E -> E
crossesUp sig val = Event (If (SigVal sig .>=. val .&. SigVal (SigDelay sig) .<. val) 
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
 "decr2" #= (LamV $ \x-> return $ x-2)
{-,
 "fact" #= (LamV $ \n -> return  (If (Var "n" .==. 1) 
                                         (1) 
                                         (M2 Mul (Var "n") (App (Var "fact") ((Var "n") -1))))) -}

 --"smap" #= LamV $ \f-> LamV $ \sig-> Sig (App (Var "f") (SigVal (Var "sig")))

          ]
              
infixl 1 #=                                         
x #= y = (x,y) 