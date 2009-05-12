{-# LANGUAGE Rank2Types #-}
module Eval where

import Expr
import EvalM
import Numbers
import Control.Monad
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

eval :: EvalS -> E -> EvalM V
eval es e@(If p c a) 
    = do pv <- vToBool =<< eval es p 
         if pv
	    then eval es c
	    else eval es a
eval es v@(Var nm) = 
    fromMaybe (lookup nm $ env es) $ ("undefined symbol: "++nm ) -- ++ "\nEnv: "++(show $ env es))

eval es (Not be) = (boolToV . not ) `fmap` (vToBool =<< eval es be)
eval es (And b1 b2) 
    = do v1 <- vToBool =<< eval es b1
	 v2 <- vToBool =<< eval es b2 
         return . boolToV $ v1 && v2
eval es (Or b1 b2) =  do 
  v1 <- vToBool =<< eval es b1
  v2 <- vToBool =<< eval es b2
  return . boolToV $ v1 || v2
eval es e@(M1 Re n) = do 
  NumV v <- eval es n
  return . NumV $ re v
eval es e@(M1 Im n) = do 
  NumV v <- eval es n
  return . NumV $ im v

eval es e@(M1 Ln n) = applyM1 es  n log  
eval es e@(M1 Exp n) = applyM1 es n exp
			
eval es e@(M2 Add n1 n2) = applyNumM2 es n1 n2 (+)
eval es e@(M2 Sub n1 n2) = applyNumM2 es n1 n2 (-)
eval es e@(M2 Mul n1 n2) = applyNumM2 es n1 n2 (*)
eval es e@(M2 Div e1 e2) = do 
  NumV v1 <- eval es e1
  NumV v2 <- eval es e2 
  return . NumV $ v1/v2

eval es (Cons e1 e2) = 
    do car <- eval es e1 
       ListV cdrl <- eval es e2
       return $ ListV (car:cdrl)
eval es (Pair e1 e2) = liftM2 (PairV) (eval es e1) (eval es e2) 
eval es (Nil) = return $ ListV []

eval es (Cmp Lt e1 e2) = applyCmp es (e1) (e2) (<) 
eval es (Cmp Gt e1 e2) = applyCmp es (e1) (e2) (>) 
eval es (Cmp Eq e1 e2) = applyCmp es (e1) (e2) (==)
eval es (Cmp Le e1 e2) = applyCmp es (e1) (e2) (<=)
eval es (Cmp Ge e1 e2) = applyCmp es (e1) (e2) (>=)
eval es (Cmp Ne e1 e2) = applyCmp es (e1) (e2) (/=)


eval es (Lam nm e) = return $ LamV (\v-> eval (extEnv (nm,v) es) e)
{-eval es env (App lam arg) t 
    = do (Lam nm bd) <- eval es env lam t
         ag <- eval es env arg t
	 eval es ((nm,ag):env) bd t-}
eval es ea@(App lam arg) 
    = do lf <- eval es lam
         let f = case lf of
                   LamV f' -> f'
                   e -> fail $ "expected lamv, got "++show e++"in expr: "++show ea
         ag <- eval es arg
	 f ag 
         --eval es env (subVar nm ag bd) t
				
{-eval es env (ShowE e) = do (StrLit . show) `fmap`  eval es env e t
eval es env (StrCat e1 e2) = do StrLit a <- eval es env e1 t
                               StrLit b <- eval es env e2 t
			       return $ StrLit (a++b) -}

{-eval es (SigVal sve) = do --Just t <- cur_t `fmap` ask
  SigV t1 t2 efun <- eval es sve
  {-case cur_t es of
    Just t -> return ()
    Nothing -> error $ "no time in expr "++show sve-}
  return $ efun (curTime es) -}

{-eval es (Sig se) 
    = do let boundVars = map fst $ env es
         if all (`elem` boundVars) fvars
            then return . SigV $ \t -> unEvalM (eval (withTime t es) se)
            else fail $ "unknown free vars: " ++ show (fvars \\ boundVars) ++ "\nEnv: "++(show $ env es)
    where fvars = freeVars se
          --boundVars = map fst env
-}

eval es (SigAt offset sve) = 
    do s<- eval (withoutTime es) sve 
       case s of 
         SigV t1 t2 efun  -> do NumV n <- eval es offset
                                let tdbl = numToDouble n
                                cond [(tdbl < t1, return (efun t1)),
                                      (tdbl > t2, return (efun t2)),
                                      (otherwise, return (efun tdbl))]
         v -> fail $ "expected sigv, got "++show v

eval es (LetE ses er) = do
  let nvs = map (\(n,e)-> (n, unEvalM $ eval (extsEnv nvs es) e)) ses
  eval (extsEnv nvs es) er

{-eval es (SigDelay s p0) = eval es (Sig $ SigAt ((SigVal (Var "seconds"))- dt') s )
    where dt' = Const . NumV . NReal $ dt es
-}
{-eval es (SigDelay s p0) 
    = return . SigV $ \t -> if round (t/dt es)==0
                               then unEvalM $ eval es p0
                               else unEvalM $ eval (withTime t es) $ SigAt ((SigVal (Var "seconds"))- dt') s
    where dt' = Const . NumV . NReal $ dt es
-}
eval es (Event evexp) = do
  let evalEvt t = case unEvalM $ eval (withTime t es) evexp of
                             ListV l -> l
                             _ -> []
  return $ ListV $ concatMap evalEvt [0,dt es..tmax es]
         
 

--Case

eval es (Box dims) = do
  dimV <- eval es dims
  return $ BoxV dimV (pair3 0 0 0) (pair3 0 0 0)

eval es (Translate xyz shp) = do
  (BoxV dims (PairV (PairV x y) z) col) <- eval es shp
  (PairV (PairV x' y') z') <- eval es xyz
  return $ BoxV dims (pair3 (x+x') (y+y') (z+z')) col

eval es (Colour col shp) = do
  (BoxV dims loc _) <- eval es shp
  (PairV (PairV r g) b) <- eval es col
  return $ BoxV dims loc (pair3 r g b)


eval es (Const v) = return v 

eval es (Case e []) = fail $ "error in pattern match"
eval es (Case e ((pat, e'):pats)) = do
  v <- eval es e
  case match pat v of
    Just env -> eval (extsEnv env es) e'
    Nothing -> eval es (Case (Const v) pats)

eval es e = fail $"unknown expr: "++show e


--applyEq :: E -> E ->(forall a. Eq a => a->a->Bool) -> E
--applyEq (CNum n1) (CNum n2) op = liftBool $ op n1 n2 

match :: Pat -> V -> Maybe [(String, V)]
match (PatVar nm) vl = Just [(nm, vl)]
match (PatIgnore) _ = Just []
match (PatLit vl) vl' | vl == vl' = Just []
                      | otherwise = Nothing
match (PatNil) (ListV []) = Just []
match (PatNil) _ = Nothing
match (PatPair px py) (PairV vx vy) = do
  boundx <- match px vx
  boundy <- match py vy
  return $ boundx++boundy
match (PatPair px py) _ = Nothing
match (PatCons pcar pcdr) (ListV (x:xs)) = do
  boundx <- match pcar x
  boundxs <- match pcdr $ ListV xs
  return $ boundx++boundxs

applyCmp :: EvalS -> E -> E ->(NumVl->NumVl->Bool) -> EvalM V
applyCmp es (e1) (e2) op
	= do 	v1 <- eval es e1 
      		v2 <- eval es e2 
		case (v1,v2) of
			(NumV n1,NumV n2) ->  return $ boolToV $ op n1 n2 
			_-> fail "foobar"

--applyCmp (b1) (b2) op = if op n1 n2 then T else F

applyM1 ::  EvalS -> E -> (forall a. Floating a => a->a) -> EvalM V
applyM1 es e f = 
    do ev <- eval es e 
       case ev of
	 NumV n -> return . NumV $ applyRealFun1 f n
	 _ -> fail "non-num arg"

applyNumM2 :: EvalS -> E -> E->  (forall a. Num a => a->a->a) -> EvalM V
applyNumM2  es e1 e2 f
	=  do 	v1 <- eval es e1 
      		v2 <- eval es e2 
		case (v1,v2) of
			(NumV n1,NumV n2) -> return . NumV $ applyNumFun2 f n1 n2
			(e1, e2) -> fail $ concat ["expected numbers, got ",
                                                   show e1 ,
                                                   " and ",
                                                   show e2,
                                                   "; FYI 9 op 8=",
                                                   show (f 9 8) ]


cond :: [(Bool, a)] -> a
cond ((True, x):_) = x
cond ((False, _):conds) = cond conds


--test = teval (1+1.5) 

teval e = unEvalM $ eval emptyEvalS e
--decr = Lam "x" $ M2 Sub (Var "x") 1 
add = Lam "x" $ Lam "y" $ M2 Add (Var "x") (Var "y") 

myAdd = App (App add 1) 2

fac 1 = 1
fac n = n * fac (n-1)

ta = teval myAdd

--t1 n = unEvalM  extEnv ("x",5) . extEnv ("y",6) $ eval (Var n)
