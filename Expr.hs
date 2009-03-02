{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Control.Monad
import Control.Monad.Reader
import Numbers
import EvalM

data Math1 = Ln | Exp | Re | Im deriving (Show, Eq)
data Math2 = Add | Sub | Mul | Div deriving (Show, Eq)
data CmpOp = Lt | Gt | Eq | Ne | Le | Ge deriving (Show, Eq)

--data EvalM a = Res {unEvalM :: a} | Error String

--newtype EvalM = Res { ReaderT Eval



instance Num V where
    (NumV n1) + (NumV n2) = NumV $n1+n2
    (NumV n1) - (NumV n2) = NumV $n1-n2
    e1 - e2 = error $ "error: "++show e1++"-"++show e2
    (NumV n1) * (NumV n2) = NumV $n1*n2
    abs (NumV n1) = NumV (abs n1)
    signum (NumV n1) = NumV (signum n1)
    fromInteger n = NumV . fromInteger $ n

 

data E =  If E E E
        | Const V
	| ShowE E
	| StrCat E E
	| Lam String E
	| App E E
	| Var String
	| Pair E E
	| Nil 
	| Cons E E
	| M1 Math1 E
	| M2 Math2 E E
	| Cmp CmpOp E E
	| And E E | Or E E | Not E
	| Case E [(Pat,E)]
	| Sig E
	| SigVal E
	| SigAt E E
        | SigDelay E
        | Event E
        | LetE [(String, E)] E
--	| NamedSig E
	deriving (Show, Eq)

data Declare 
	= Let String E
--	| LetEvt String E
	| LetRec String E
	| Import String (Maybe String)
	| SinkConnect E String
	deriving (Show, Eq)

queryE :: (E-> [a]) -> E -> [a]
queryE q e@(If p c a) = q e ++ m p ++ m c ++m a
	where m = queryE q

queryE q e@(LetE ses er) = q e ++ m er ++ concatMap (m . snd) ses  
	where m = queryE q

queryE q e@(Lam n bd) = q e ++ m bd
	where m = queryE q
queryE q e@(App le ae) = q e ++ m le ++ m ae
	where m = queryE q
queryE q e@(Pair e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Cons e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(M1 _ e1) = q e++m e1
	where m = queryE q
queryE q e@(M2 _ e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Cmp _ e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(And e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Or e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Not e1) = q e++m e1
	where m = queryE q
queryE q e@(Sig e1) = q e++m e1
	where m = queryE q
queryE q e@(SigVal e1) = q e++m e1
	where m = queryE q
queryE q e@(SigDelay e1) = q e++m e1
	where m = queryE q
queryE q e@(Event e1) = q e++m e1
	where m = queryE q
queryE q e@(Const _) = q e
	where m = queryE q
queryE q e@(SigAt e1 e2) = q e++m e1++m e2
	where m = queryE q
queryE q e@(Case ce cs) = q e++q ce++concatMap (m . snd) cs
	where m = queryE q
queryE q e@(Var _) = q e
queryE q e@(Nil) = q e
queryE q e = error $ "queryE: unknown expr "++show e 

freeVars :: E -> [String]
freeVars e = fv [] e
    where fv e (If p c a) = fv e p ++ fv e c ++ fv e a
          fv e (Lam n bd) = fv (n:e) bd
          fv e (App lm ar) = fv e lm ++ fv e ar
          fv e (Var n) | n `elem` e = []
                       | otherwise = [n]
          fv e (Sig s) = fv e s
          fv e (SigVal s) = fv e s
          fv e (SigAt s1 s2 ) = fv e s1 ++ fv e s2
          fv e (M1 _ s) = fv e s
          fv e (M2 _ s1 s2 ) = fv e s1 ++ fv e s2
          fv e (Not s) = fv e s
          fv e (Cmp _ s1 s2) = fv e s1 ++ fv e s2
          fv e (And s1 s2) = fv e s1 ++ fv e s2
          fv e (Or s1 s2) = fv e s1 ++ fv e s2
          fv e (Cons s1 s2) = fv e s1 ++ fv e s2
          fv e (Pair s1 s2) = fv e s1 ++ fv e s2
          fv e (SigDelay s1) = fv e s1
          fv e (Event s1) = fv e s1
          fv e (Const _) = []
          fv e (Nil) = []
          fv e (LetE ses er) = fv (map fst ses++e) er ++ concatMap (fv (map fst ses++e) . snd) ses 
          --fv e (expr) = []

mapE :: (E-> E)-> E -> E
mapE f (If p c a) = f (If (m p) (m c) (m a))
    where m = mapE f 
mapE f (Lam n bd) = f (Lam n (m bd))
    where m = mapE f
mapE f (App le ae) = f (App (m le) (m ae))
    where m = mapE f
mapE f (Var n) = f (Var n)
mapE f (Sig s) = f (Sig (mapE f s))
mapE f (SigVal s) = f (SigVal (mapE f s))
mapE f (SigDelay s) = f (SigDelay (mapE f s))
mapE f (SigAt s1 s2) = f (SigAt (mapE f s1) (mapE f s2))
mapE f (M1 m s) = f (M1 m (mapE f s))
mapE f (M2 m s1 s2) = f (M2 m (mapE f s1) (mapE f s2))
mapE f (And s1 s2) = f (And (mapE f s1) (mapE f s2))
mapE f (Or s1 s2) = f (Or (mapE f s1) (mapE f s2))
mapE f (Cons s1 s2) = f (Cons (mapE f s1) (mapE f s2))
mapE f (Not s) = f (Not (mapE f s))
mapE f (Cmp o s1 s2) = f (Cmp o (mapE f s1) (mapE f s2))
mapE f (Pair s1 s2) = f (Pair (mapE f s1) (mapE f s2))
mapE f (Event s2) = f (Event (mapE f s2))
mapE f (Const c) = f (Const c)
mapE f (Nil) = f (Nil)
mapE f (LetE ses er) = f (LetE (map (\(n,e)-> (n, mapE f e)) ses) (mapE f er))
mapE f e = error $ "mapE: unknown expr "++show e 

--mapE f e = f e

subVar n es e = mapE f e where 
    f (Var n') | n== n' = es
               | otherwise = Var n'
    f x = x

instance Num E where
	e1 + e2 = M2 Add e1 e2
	e1 - e2 = M2 Sub e1 e2
	e1 * e2 = M2 Mul e1 e2
	negate e = M2 Mul (-1) e
	abs e = If (Cmp Lt e 0) (negate e) (e)
	signum e = If (Cmp Lt e 0) (-1) (1)
	fromInteger i = Const . NumV $ NInt (fromInteger i)

instance Fractional E where
	e1 / e2 = M2 Div e1 e2
	fromRational r = Const . NumV . NReal $ fromRational r


data Pat = 	  PatAny String
		| PatIgnore
		| PatLit E
		| PatPair Pat Pat
		| PatNil
		| PatCons Pat Pat
		| PatGuard E Pat
		deriving (Show, Eq)

--sugar

infixl 6 .>.
infixl 6 .<.
infixl 6 .>=.
infixl 6 .<=.
infixl 5 .&.
infixl 4 .|.


e1 .>. e2 = Cmp Gt e1 e2
e1 .<. e2 = Cmp Lt e1 e2
e1 .>=. e2 = Cmp Ge e1 e2
e1 .<=. e2 = Cmp Le e1 e2
e1 .==. e2 = Cmp Eq e1 e2




e1 .&. e2 = And e1 e2
e1 .|. e2 = Or e1 e2

($>) = App


t = Const $ BoolV True
f = Const $ BoolV False


class ToE a where
    toExpr :: a -> E

instance ToE E where
    toExpr = id

instance ToE [Char] where
    toExpr = Var

sig :: ToE a => a -> E
sig = Sig . toExpr

sigVal :: ToE a => a -> E
sigVal = SigVal . toExpr

sigDelay :: ToE a => a -> E
sigDelay = SigDelay . toExpr

x ^$> y = (toExpr x) $> (toExpr y)