{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Proof.Equal where

import Baysig.Expr
import Baysig.Eval
import Baysig.Proof.Core
import Control.Monad
import Data.Maybe

--apTerm f (C |- a = b) => C |- (f a) = (f b)
apTerm tm th = refl tm >>= \ref -> mkApp ref th

--apTheorem (C |- f = g) a => C |- (f a) = (g a)
apTheorem thm term = refl term >>= mkApp thm

--sym (C |- a=b) => C |- b=a)
--sym thm = 
evalRule :: ([(String,E)],[(String,[T])]) -> E -> Either String Theorem
evalRule env e@(EApp fe arge) = evRCons env e `mplus` do 
   (thmf, f) <- with_rhs $ evalRule env fe 
   (thmarg, arg) <- with_rhs $ evalRule env arge
   (bc, e'') <- with_rhs $  betaConv (EApp f arg)
   appeq <- mkApp thmf thmarg 
   er <- evalRule env e''   
   bc_er <- trans bc er
   trans appeq bc_er
evalRule env e@(ECase _ pates) = do
   (mtch, e') <- with_rhs $ match e
   er <- evalRule env e'
   trans mtch er
evalRule env e@(EVar nm) = (do
   ts <- lookupM nm (snd env)
   refl e) `mplus` (do
   defe <- lookupM nm $ fst env
   athm <- assume (e `eq` defe)
   er <- evalRule env defe
   trans athm er) `mplus` refl e
evalRule env e = refl e 

evRCons :: ([(String,E)],[(String,[T])]) -> E -> Either String Theorem
evRCons env@(defs, constrs) e = msum $ map f constrs where
   f:: (String,[T]) -> Either String Theorem 
   f (nm,[]) = if e == EVar nm
                  then refl e
                  else mzero
   f (nm, [t]) 
     = case e of
         EApp (EVar nm') arg -> do
           guard (nm==nm')
           (argthm, argv) <- with_rhs $ evalRule env arg
           eqf <- refl (EVar nm)
           mkApp eqf argthm
         _ -> mzero


with_rhs mthm = do
   thm <- mthm
   (_, (_,x)) <- veq thm
   return (thm, x)