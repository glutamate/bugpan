{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Proof (Theorem, v, TheoremView((:-)), 
                     refl, assume, trans, mkApp, 
                     abstract, eqMP, betaConv) where

import Baysig.Expr
import Control.Monad

data Theorem = Thm [E] E 

data TheoremView = [E] :- E 
v (Thm es e) = es :- e

eq x y = EVar "==" $> x $> y
splitEq (EApp (EApp (EVar "==") x) y) = return (x,y)
splitEq _ = mzero

refl e = return $ Thm [] (eq e e )
assume e = return $ Thm [e] e
trans (Thm asm1 e1) (Thm asm2 e2) = do
  (a, b) <- splitEq e1
  (b', c) <- splitEq e2
  guard (b==b')
  return (Thm (asm1++asm2) (eq a c)) 
mkApp (Thm asm1 e1) (Thm asm2 e2) = do
  (f@(ELam _ _), g@(ELam _ _)) <- splitEq e1
  (a, b) <- splitEq e2
  return (Thm (asm1++asm2) (eq (f $> a) (g $> b)))
abstract x (Thm asm e) = do
  (a, b) <- splitEq e
  guard (any (x `isFreeVarIn`) asm)
  return (Thm asm $ eq (ELam (PVar x) a) (ELam (PVar x) a))
eqMP (Thm asm1 e) (Thm asm2 a) = do
  (a', b) <- splitEq e
  guard (a==a')
  return (Thm (asm1++asm2) b)
betaConv ap@(EApp (ELam (PVar x) a) y) =
  return $ Thm [] $ eq ap $ subVar x y a
betaConv _ = mzero

--BETA_CONV `(\x. A) y`
--gives `|- (\x. A) y = A[y/x]`
  
