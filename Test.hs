module Main where

import Expr
import Eval
import EvalM
import Run
import Control.Monad
import Numbers
import Data.Char

main = ppso 
-- {-lookup "cross1" `fmap`-} run preludeFFI testProg testdt testtmax
-- :set -fbreak-on-exception


testdt, testtmax :: Floating a => a
testdt = 0.001
testtmax = 0.06

ppe = pp

mypp n = putStrLn $ pp $ getE n testProg
ppso = mypp "solveOde'"

pp1 =  putStrLn $ map chr [0x03bb,0x644,32,0x62A,0x62C,0x62F,0x646,32, 0xceb1]


getE :: String -> [Declare] -> E
getE n [] = error "Not Found"
getE n ((Let n1 e):tl) | n == n1 = e
                       | otherwise = getE n tl
getE n (_:tl) = getE n tl



testProg = prelude ++
    [Let "secs" (Var "seconds"),
     Let "secsp1" $ (Var "smap") $> (Var "incr") $> (Var "seconds"),
     Let "secsp1d2" $ Sig (SigAt (time-0.2) (Var "secsp1")),
     Let "secsp1d1" $ SigDelay (Var "secsp1") 15,
     --Let "intsig" $ Var "integrate" $> (Sig 1), 

     Let "accsig" (Var "sscan" $> Var "add" $> (-5) $> Sig 1),

     --Let "add34" (Var "add" $> 3 $> 4),
     Let "syn1" (Var "alpha" $> 10 $> 1),
     Let "gsyn" (Var "smap" $> (Var "alpha" $> 300) $> (Var "seconds")),
     Let "intsyn" $ Var "integrate" $> (Var "gsyn"),
      "preSpike" =: (Var "every" $> 0.01),
     "sum_1_10" =: (Var "sum" $> (Var "oneToTen")),
     "gcell" =: (Var "convolve" $> Var "gsyn" $> Var "preSpike"),
     "isig" =: (Var "smap" $> (Var "mul" $> 1e-14 ) $> (Var "gcell")),
     "isig2" =: (Var "step" $> 2 $> 4),
     "isig1" =: (Sig 10e-12),
     "anumber" =: 5,
     "cellOde" =: (Lam "v" $ Sig $ ((SigVal $ Var "isig")-((Var "v"+0.07)/1e9))/(2.5e-12)),
     --"cellOde" =: (Lam "v" $ Sig $ ((SigVal $ Var "isig")-((-0.07)/1e9))/2e5-12),
     "simpleOde" =: (Lam "y" $ Sig $ (SigVal (Var "step1")-(Var "y"))),
     "v" =: (Var "solveOde" $> Var "cellOde" $> (-0.07)),
     "s" =: (Var "solveOde1" $> Pair (testdt) (testtmax) $> Var "cellOde" $> (-0.07)),
     --Let "cross1" $ crossesUp (Var "seconds") 0.5,
     --Let "fact5" (Var "fact" $> 5),
     --Let "fib6" (Var "fib" $> 6),
     --Let "cross2" $ crossesUp (Var "secsp1d1") 1.5,
     --Let "fib5" (Var "fib" $> 5),
     --Let "iterIncr" $ (Var "iterate" $> Var "incr" $> 1.0),
     SinkConnect (Var "s") "plot"]

-- Dv = (i-v/r)/c 

--Dv=3

{-test = teval (1+1.5) 

teval e = unEvalM emptyEvalS $ eval e
add = Lam "x" $ Lam "y" $ M2 Add (Var "x") (Var "y") 
myAdd = App (App add 1) 2

ta = teval myAdd

t1 n = unEvalM emptyEvalS $ extEnv ("x",5) . extEnv ("y",6) $ eval (Var n)


-}

{-tA = tstEval (Var "alpha" $> 0.39 $> 1)
tN = tstEval (negate 3)

test = testExprs [
        Var "alpha" $> 0.39 $> 1 #= (NumV $ 0.1029803506111),
        negate 3 #= (-3),
        (Var "sum" $> (Var "oneToTen")) #= 55

       ]

tEq1 = (12.000001::E) == (12.000002::E)
tEq2 = (12.1::E) == (12.2::E)

testExprs :: [(E,V)] -> IO ()
testExprs [] = return ()
testExprs ((e,v):rest) = case sfEvalM $ tstEval e of
                           Right v' -> when (v'/=v) $ putStrLn $ show e++"="++show v'++", expected "++show v
                           Left err -> putStrLn $ "error in eval "++show e++": "++err

-}
--convolve :: E -> E -> E
--convolve s es = Sig $ foldr (+) $ map fst e --see arraywave

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


snapshot sig evt =   Var "map" $> Lam "tvp" (SigAt (Var "fst" $> Var "tvp") sig) $> evt
time = SigVal (Var "seconds")

--letEvt "foo" 

decr = Var "decr"

preludeFFI :: Env
preludeFFI = [
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
 --"oneToTen" #= (ListV . map (NumV . NInt) $ [1..10]),
 "sum" #= (LamV $ \vs -> foldr (+) 0 `fmap` unListV vs),
 "step1" #= (SigV (\t-> NumV . NReal $ if t>2 && t<4 then 1 else 0)),
 "every" #= (LamV $ \(NumV iv) -> return $ ListV [PairV (NumV $ NReal tm) Unit | tm <- [0,(numToDouble iv)..100]]),
 "solveOde1" #= (LamV $ \(PairV dtV tmaxV)->return $ LamV $ \(LamV sf)-> return $ LamV $ \y0 -> do 
                                              dt <- vToDbl dtV
                                              tmax <- vToDbl tmaxV
                                              vls <- scanM (\y t-> do
                                                              (SigV sig) <- sf y
                                                              let deriv = sig $ t-dt
                                                              return $ y+(NumV . NReal $ dt)*deriv
                                                              ) y0 $ [0,dt..tmax]
                                              return $ SigV $ \t-> vls !! round (t/dt)
                )]


 -- http://www.haskell.org/pipermail/beginners/2009-January/000667.html
scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)


constReal = Const . NumV . NReal 
constInt = Const . NumV . NInt

--l = [PairV (Const . NumV . NReal $ tm) Unit | tm <- [0,(numToDouble intvl)..]]

prelude :: [Declare]
prelude = [
 "alpha" =: (Lam "tau" . Lam "t" $ If (Var "t" .<. 0) 0 (tau*tau*t *exp (negate tau * t))),
 
 "add" =: (Lam "x" $ Lam "y" $ Var "x" + Var "y") ,
 "mul" =: (Lam "x" $ Lam "y" $ Var "x" * Var "y") ,
 "fact"=: (Lam "n" $ If (Var "n" .==. 1) 
                               1 
                               (Var "n" * (Var "fact" $> (Var "n" -1)))),
 "iterate" =: (Lam "f" $ Lam "s0" $ 
          LetE ["s" #= ( 
                        Sig (If (time .<=. 0.05) 
                                    (Var "s0")
                                    (Var "f" $> SigVal (SigDelay (Var "s") (Var "s0"))))
                       )] (Var "s")),
 "smap" =: (Lam "f" . Lam "s" $ Sig (Var "f" $> (SigVal $ Var "s"))),

 "sscan" =: (Lam "f" . Lam "v0" . Lam "s" $
                    LetE ["sr"#= (Sig $ (Var "f") $> (SigVal (Var "s")) $> (SigVal $ SigDelay (Var "sr") (Var "v0")))
                        ] $ Var "sr"),
 "intStep'" =: (Lam "new" . Lam "otp" $ Pair (("fst" ^$> "otp") + (Var "new")*(time-("snd" ^$> "otp"))) (time)),

 "integrate'" =: (Lam "s" $ (Var "smap" $> Var "fst") $> (Var "sscan" $> (Var "intStep") $> (Pair 0 0) $> Var "s")),

 "fib" =: (LetE ["f" #= (Lam "n" $ If (Var "n" .<. 3) 
                                              (1) 
                                              ((Var "f" $> (decr $> (Var "n")))+(Var "f" $> (Var "decr2" $> (Var "n")))))] 
           (Var "f")),
 "step" =: (Lam "t1" . Lam "t2" $ Sig $ If (((Var "t1") .<=. time) .&. ((Var "t2") .>. time)) (1) (0)),

 "accum" =: (Lam "s" $ Var "sscan" $> Var "add" $> 0 $> Var "s"),

 "integrate" =: (Lam "s" $ (Var "sscan" $> (Var "intStep") $> (0) $> Var "s")),
 "intStep" =: (Lam "new" . Lam "old" $ (Var "old") + (Var "new")*(Var "fixedDt")),
 "convolve" =: 
     (Lam "s" . Lam "es" $ Sig (Var"sum" $> (Var "map" $> (Lam "e" (SigAt (time- (Var "fst" $> Var "e" )) (Var "s"))) $> Var "es"))),
 "solveOde'" =: (Lam "sf" . Lam "v0" $
               (LetE ["s" #= (Sig $ SigVal (SigDelay (Var "s") (Var "v0")) + 
                                            dt * (SigVal $ (Var "sf" $> (SigVal $ SigDelay (Var "s") (Var "v0")))) )] 
                (Var "s"))),
 "solveStep" =: (Lam "v0" . Lam "sf" . Lam "old" $ (Var "old") + ( SigVal (SigDelay (Var "sf" $> Var "old") (Var "v0"))) * Var "fixedDt"),
 "solveOde" =: (Lam "sf" . Lam "v0" $ Var "iterate" $> (Var "solveStep" $> Var "v0" $> Var "sf") $> Var "v0")]

--solve = \sf v0 -> let s = Sig $ (sigval (delay s v0) + dt * (sf (sigval (delay s v0)))@t-dt) in s
--solveStep sf new old = old + dt * (sf old) -- approx!
--solve sf v0 = iterate (solvestep sf) v0

 -- "every" =: (Lam "interval" $ 

    where tau = Var "tau"
          t = Var "t"
          dt = Var "fixedDt"
--convolve s es = Sig . sum .map (\timp->SigAt (time-timp) s ) . map fst $ es --see arraywave
--solveODE :: (a->Signal) -> a -> Signal
{-solveODE sf v0 = s
                 where s = Sig $ SigVal (SigDelay s v0) + dt * (SigAt (time-dt) $ sf (SigVal (SigDelay s v0)) )
                       dt = 0.1
    
preludeSt = extsEnv preludeFFI emptyEvalS     

tstEval = eval wffi
          where wffi =  extsEnv preludeFFI emptyEvalS  
                -- wprelude = eval wffi prelude

-}
infixl 1 #=                                         
x #= y = (x,y) 

infixl 1 =:                                         
x =: y = Let x y 
