module Types where

import Expr
import EvalM
import Numbers
import Control.Monad
import PrettyPrint
import Data.Maybe

type TEnv = [(String, T)]



exprType :: TEnv -> E -> Maybe T -> Maybe T
exprType e (If p c a) t = do pt <- exprType e p (Just BoolT)
                             unifyTypes BoolT pt
                             ct <- exprType e c t
                             at <- exprType e a t
                             unifyTypes ct at

exprType e (Const (NumV (NInt _))) _ = Just $ NumT (Just IntT)
exprType e (Const (NumV (NReal _))) _ = Just $ NumT (Just RealT)
exprType e (Const (NumV _)) _ = Just $ NumT Nothing
exprType e (Const (BoolV _)) _ = Just BoolT
--exprType e (Lam nm t ex) = LamT t `fmap` exprType ((nm,t):e) ex
exprType e (Var nm) _ = lookup nm e
exprType e (App le arge) (Just declT)
    = do argt <- exprType e arge (Just $ AnyT)
         lt <- exprType e le (Just $ LamT argt declT)
         (LamT _ rt) <-unifyTypes lt (LamT argt AnyT)
         return rt

exprType e (Pair e1 e2) _ = liftM2 (PairT) (exprType e e1 Nothing) (exprType e e1 Nothing)

exprType e (Nil) _ = Just $ ListT AnyT

exprType e (Cons hd tl) (Just (ListT decty))
    = do ht <- exprType e hd (Just decty)
         ListT tt <- exprType e tl (Just $ ListT decty)
         ListT `fmap` unifyTypes (ht) (tt)
exprType e (Case ex pts) _
    = do (pat:pats) <- sequence $ map (\(x,y) -> exprType e y Nothing) pts
         foldM (unifyTypes) pat pats
exprType e (Lam nm t bd) (Just (LamT dnmt dbdt)) = do 
  tbd <- exprType ((nm, dnmt):e) bd (Just dbdt)
  realBdt <- unifyTypes tbd dbdt
  return $ LamT dnmt realBdt
exprType e (M1 op ne) dt = do
  neTy <- exprType e ne dt
  unifyTypes (NumT Nothing) neTy
exprType e (M2 op ne1 ne2) declt = do
  ne1Ty <- exprType e ne1 declt
  ne2Ty <- exprType e ne2 declt
  neTy <- unifyTypes ne2Ty ne1Ty
  unifyTypes (NumT Nothing) neTy
              
exprType env (LetE [(PatVar nm t,e)] (Var nm')) declt | nm == nm' = t
  where t = exprType ((nm,fromJust t):env) e declt   
  
exprType env (Sig se) (Just (SignalT sigt)) = do
  SignalT `fmap` exprType env se (Just sigt)

exprType env (Sig se) Nothing = do
  SignalT `fmap` exprType env se Nothing

exprType env (SigAt t s) (Just decty) = do
  tt<- exprType env t (Just $ NumT (Just RealT))
  unifyTypes tt (NumT $ Just RealT)
  st<- exprType env s (Just $ SignalT decty)
  unifyTypes st  (SignalT decty)
  return decty

exprType env (SigVal s) (Just decty) = do
  st<- exprType env s (Just $ SignalT decty)
  unifyTypes st  (SignalT decty)
  return decty

exprType e ex Nothing = error $ "unknown expr: "++ pp ex
exprType e ex (Just decl) = error $ "unknown expr: "++ pp ex++" declared: "++show decl
--and or not
--M1 m2
--cmp
--exprType r (Lam bd arg) (Just t) =

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

unifyTypes (SignalT t1) (SignalT t2) =  SignalT `fmap` unifyTypes t1 t2

unifyTypes (NumT (Just nt1)) (NumT (Just nt2)) = Just $ NumT (Just $ max nt1 nt2)

unifyTypes (NumT Nothing) (NumT nt) = Just $ NumT nt
unifyTypes (NumT nt) (NumT Nothing) = Just $ NumT nt

unifyTypes (TyVar x) (TyVar y) | x == y = Just $ TyVar x
                               | otherwise = Nothing

unifyTypes t1 t2 | t1 == t2 = Just t1
                 | otherwise = Nothing

haskTypeString :: T -> String
haskTypeString BoolT = "Bool"
haskTypeString UnitT = "()"
haskTypeString (PairT t1 t2) = "("++haskTypeString t1++","++haskTypeString t2++")"
haskTypeString (LamT t1 t2) = "("++haskTypeString t1++")->("++haskTypeString t2++")"
haskTypeString (ListT t1) = "["++haskTypeString t1++"]"
haskTypeString (NumT (Just RealT)) = "Double"
haskTypeString (NumT (Just IntT)) = "Int"
haskTypeString (NumT Nothing) = "Number"
haskTypeString (SignalT t) = "Signal "++haskTypeString t
haskTypeString (TyVar s) = s
haskTypeString (ShapeT) = "Shape Double"
haskTypeString (StringT) = "String"
haskTypeString (EventT t) = "[(Double, "++haskTypeString t++")]"
haskTypeString t = error $ "haskTypeString: unknown type "++show t

isSubtypeOf:: T -> T -> Bool
isSubtypeOf (NumT _) (NumT Nothing) = True
isSubtypeOf t1 t2 | t1 == t2 = True
                  | otherwise = False
