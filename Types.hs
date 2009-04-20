module Types where

import Expr

data T  = BoolT
	| NumT
	| PairT T T
	| LamT T T
	| ListT T
	| AnyT
	| StringT
	| SignalT T
	| EventT T
	| EpochT T
	deriving (Show, Eq)

type TEnv = [(String, T)]

exprType :: TEnv -> E -> Maybe T
exprType e (If p c a) = do pt <- exprType e p
                           unifyTypes BoolT pt
                           ct <- exprType e c
                           at <- exprType e a
                           unifyTypes ct at

exprType e (CNum _) = Just NumT
exprType e (T) = Just BoolT
exprType e (F) = Just BoolT
--exprType e (Lam nm t ex) = LamT t `fmap` exprType ((nm,t):e) ex
exprType e (Var nm) = lookup nm e
exprType e (App le arge) 
    = do lt <- exprType e le
         argt <- exprType e arge
         (LamT _ rt) <-unifyTypes lt (LamT argt AnyT)
         return rt

exprType e (Pair e1 e2) = liftM2 (PairT) (exprType e e1) (exprType e e1)

exprType e (Nil) = Just $ ListT AnyT

exprType e (Cons hd tl) 
    = do ht <- exprType e hd
         tt <- exprType e tl
         ListT `fmap` unifyTypes (ht) (tt)
exprType e (Case ex pts) 
    = do (pat:pats) <- sequence $ map (exprType e . snd) pts
         foldM (unifyTypes) pat pats
--and or not
--M1 m2
--cmp

unifyTypes :: T -> T -> Maybe T
unifyTypes AnyT t = return t
unifyTypes t AnyT = return t

unifyTypes (PairT t11 t12) (PairT t21 t22) 
    = do t1 <- unifyTypes t11 t21 
         t2 <- unifyTypes t12 t22 
         return (PairT t1 t2)
unifyTypes (LamT t11 t12) (LamT t21 t22) 
    = do t1 <- unifyTypes t11 t21 
         t2 <- unifyTypes t12 t22 
         return (LamT t1 t2)

unifyTypes (ListT t1) (ListT t2) = ListT `fmap` unifyTypes t1 t2

unifyTypes t1 t2 | t1 == t2 = Just t1
                 | otherwise = Nothing