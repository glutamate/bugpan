{-# LANGUAGE Rank2Types #-}
module EvalCPS where

import Expr
import EvalM
import Numbers
import Control.Monad
import Data.Maybe
import Control.Monad.Reader
import Data.List

--type EvalM = Maybe

 
-- instance Functor EvalM where
--     fmap f (Res x) = Res $ f x
--     fmap _ (Error s) = Error s

-- instance Monad EvalM where
--     return x = Res x
--     Res x >>= f = f x
--     Error s >>= _ = Error s 
--     fail s = Error s

-- instance MonadPlus EvalM where
--     mzero = Error "mzero"
--     Res x `mplus` _ = Res x
--     Error s `mplus` y = y

-- instance Show a => Show (EvalM a) where
--     show (Res x) = show x
--     show (Error s) = "error: "++s


{-sumEvalM :: [EvalM a b] -> Either a [b]
sumEvalM es = foldr f (Right []) es
    where f (Right x) (Right xs) = Right (x:xs) 
          f _ (Left e) = Left e
          f (Left e) _ = Left e 
-}

eval :: EvalS -> E -> (V->a) -> a
eval es e@(If p c a) k
    = eval es p (\(BoolV b)-> if b 
                                then eval es c k
                                else eval es a k)

{-do pv <- vToBool =<< eval es p 
         if pv
	    then eval es c
	    else eval es a-}
eval es v@(Var nm) k = 
   k (fromJust . lookup nm $ env es) -- $ "undefined symbol: "++nm -- ++ "\nEnv: "++show env

eval es (Not be) k = eval es be $ \(BoolV b) -> k (BoolV $ not b) -- (boolToV . not ) `fmap` (vToBool =<< eval es be)
eval es (And be1 be2) k
    = eval es be1 $ \(BoolV b1) -> 
      eval es be2 $ \(BoolV b2) -> 
      k . BoolV $ b1 && b2
eval es (Or be1 be2) k
    = eval es be1 $ \(BoolV b1) -> 
      eval es be2 $ \(BoolV b2) -> 
      k . BoolV $  b1 || b2

{-do v1 <- vToBool =<< eval es b1
	 v2 <- vToBool =<< eval es b2 
         return . boolToV $ v1 && v2-}


eval es e@(M1 Re n) k = eval es n $ \(NumV v)-> k $ NumV $ re v
eval es e@(M1 Im n) k = eval es n $ \(NumV v)-> k $ NumV $ im v
eval es e@(M1 Ln n) k = applyM1 es  n log k
eval es e@(M1 Exp n) k = applyM1 es n exp k
			
eval es e@(M2 Add n1 n2) k = applyNumM2 es n1 n2 (+) k
eval es e@(M2 Sub n1 n2) k = applyNumM2 es n1 n2 (-) k
eval es e@(M2 Mul n1 n2) k = applyNumM2 es n1 n2 (*) k


eval es e@(M2 Div e1 e2) k = 
      eval es e1 $ \(NumV n1) -> 
      eval es e2 $ \(NumV n2) -> 
      k . NumV $ n1/n2

eval es (Cons e1 e2) k = 
      eval es e1 $ \car -> 
      eval es e2 $ \(ListV cdr) -> 
      k $  ListV (car:cdr)

eval es (Pair e1 e2) k = 
      eval es e1 $ \(v1) -> 
      eval es e2 $ \(v2) -> 
      k $ PairV v1 v2 
eval es (Nil) k = k $ ListV []

eval es (Cmp Lt e1 e2) k = applyCmp es (e1) (e2) (<) k
eval es (Cmp Gt e1 e2) k = applyCmp es (e1) (e2) (>) k
eval es (Cmp Eq e1 e2) k = applyCmp es (e1) (e2) (==) k
eval es (Cmp Le e1 e2) k = applyCmp es (e1) (e2) (<=) k
eval es (Cmp Ge e1 e2) k = applyCmp es (e1) (e2) (>=) k
eval es (Cmp Ne e1 e2) k = applyCmp es (e1) (e2) (/=) k
 

eval es (Lam nm e) k1 = k1 $ ULamV (\v k2-> eval (extEnv (nm,v) es) e k2) 
{-eval es env (App lam arg) t 
    = do (Lam nm bd) <- eval es env lam t
         ag <- eval es env arg t
	 eval es ((nm,ag):env) bd t-}


eval es ea@(App lam arg) k
    = eval es lam $ \(ULamV lf)->
      eval es arg $ \ag ->lf ag k
         --eval es env (subVar nm ag bd) t
	

{-eval es env (ShowE e) = do (StrLit . show) `fmap`  eval es env e t
eval es env (StrCat e1 e2) = do StrLit a <- eval es env e1 t
                               StrLit b <- eval es env e2 t
			       return $ StrLit (a++b) -}

eval es (SigVal sve) k =  --Just t <- cur_t `fmap` ask
  eval es sve $ \(SigV efun) ->
  {-case cur_t es of
    Just t -> return ()
    Nothing -> error $ "no time in expr "++show sve-}
  k $ efun (curTime es)


eval es (Sig se) k
    = let boundVars = map fst $ env es in
      if all (`elem` boundVars) fvars
            then k . USigV $ \t k2-> eval (withTime t es) se k2
            else error $ "unknown free vars: " ++ show (fvars \\ boundVars) ++ "\nEnv: "++(show $ env es)
    where fvars = freeVars se
          --boundVars = map fst env
{-
 
eval es (SigAt offset sve) = 
    do SigV efun <- eval (withoutTime es) sve 
       NumV n <- eval es offset
       return (efun $ (numToDouble n)) 
-}
eval es (LetE ses er) k = 
  let nvs = map (\(n,e)-> (n, eval (extsEnv nvs es) e) k) ses in
  eval (extsEnv nvs es) er k

{-

{-eval es (SigDelay s p0) = eval es (Sig $ SigAt ((SigVal (Var "seconds"))- dt') s )
    where dt' = Const . NumV . NReal $ dt es
-}
eval es (SigDelay s p0) 
    = return . SigV $ \t -> if round (t/dt es)==0
                               then unEvalM $ eval es p0
                               else unEvalM $ eval (withTime t es) $ SigAt ((SigVal (Var "seconds"))- dt') s
    where dt' = Const . NumV . NReal $ dt es

eval es (Event evexp) = do
  let evalEvt t = case unEvalM $ eval (withTime t es) evexp of
                             ListV l -> l
                             _ -> []
  return $ ListV $ concatMap evalEvt [0,dt es..tmax es]
         


--Case


eval es (Const v) = return v 
eval es e = fail $"unknown expr: "++show e

-}

--applyEq :: E -> E ->(forall a. Eq a => a->a->Bool) -> E
--applyEq (CNum n1) (CNum n2) op = liftBool $ op n1 n2 

applyCmp :: EvalS -> E -> E ->(NumVl->NumVl->Bool) -> (V-> b) -> b
applyCmp es (e1) (e2) op k
	=  eval es e1 $ \(NumV n1)-> 
           eval es e2 $ \(NumV n2)-> 
	   k . BoolV $ op n1 n2




applyM1 ::  EvalS -> E -> (forall a. Floating a => a->a) -> (V-> b) -> b
applyM1 es e f k = 
    eval es e $ \(NumV n)->
        k . NumV $ applyRealFun1 f n
	 

applyNumM2 :: EvalS -> E -> E->  (forall a. Num a => a->a->a)  -> (V-> b) -> b
applyNumM2  es e1 e2 f k
	=  eval es e1 $ \(NumV n1)-> 
           eval es e2 $ \(NumV n2)-> 
	   k . NumV $ applyNumFun2 f n1 n2

--test = teval (1+1.5) 
{-
teval e = unEvalM $ eval emptyEvalS e
--decr = Lam "x" $ M2 Sub (Var "x") 1 
add = Lam "x" $ Lam "y" $ M2 Add (Var "x") (Var "y") 

myAdd = App (App add 1) 2

fac 1 = 1
fac n = n * fac (n-1)

ta = teval myAdd

--t1 n = unEvalM  extEnv ("x",5) . extEnv ("y",6) $ eval (Var n)
-}

