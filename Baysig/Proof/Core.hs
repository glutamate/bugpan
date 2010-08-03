{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Proof.Core (
                     Theorem, v, veq, eq, 
                     refl, assume, trans, mkApp, 
                     abstract, eqMP, betaConv, match) where

import Baysig.Expr
import Control.Monad
import Baysig.Eval (matchE)
import Data.List 

data Theorem = Thm [E] E 

v (Thm asm e) = (asm, e)
veq (Thm asm e) = splitEq e >>= return . (,) asm

eq x y = EVar "==" $> x $> y
splitEq (EApp (EApp (EVar "==") x) y) = return (x,y)
splitEq e = fail "splitEq"

asm1 +++ asm2 = nub $ asm1 ++ asm2

refl e = return $ Thm [] (eq e e )
assume e = return $ Thm [e] e
trans (Thm asm1 e1) (Thm asm2 e2) = do
  (a, b) <- splitEq e1
  (b', c) <- splitEq e2
  if (b==b')
     then return (Thm (asm1+++asm2) (eq a c))
     else fail "trans" 
mkApp (Thm asm1 e1) (Thm asm2 e2) = do
  (f, g) <- splitEq e1
  (a, b) <- splitEq e2
  return (Thm (asm1++asm2) (eq (f $> a) (g $> b)))
abstract x (Thm asm e) = do
  (a, b) <- splitEq e
  if any (x `isFreeVarIn`) asm
     then fail "abstract: not free"
     else return (Thm asm $ eq (ELam (PVar x) a) (ELam (PVar x) a))
eqMP (Thm asm1 e) (Thm asm2 a) = do
  (a', b) <- splitEq e
  if a==a'
     then return (Thm (asm1+++asm2) b)
     else fail "eqMp"
betaConv ap@(EApp (ELam (PVar x) a) y) =
  return $ Thm [] $ ap `eq` subVar x y a
betaConv _ = fail "betaConv"
inst n e (Thm asm conse) = do
  let subf = subVar n e 
  return $ Thm (map subf asm) $ subf conse
match e = match' e e 
match' e1 (ECase v []) = fail $ "match: no match "++show e1
match' e1 (ECase v ((p,e):pates)) = (do
   exts <- matchE p v
   return $ Thm [] (e1 `eq` subEnv exts e) 
   ) `mplus` match' e1 (ECase v pates) 
match' e1 _ = fail $ "match fail:"++show e1

subEnv :: [(String, E)] -> E -> E
subEnv [] e = e
subEnv ((nm,v):env) e = subEnv env $ subVar nm v e