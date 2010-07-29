{-# LANGUAGE TypeOperators #-}

module Baysig.Proof (Theorem, v, TheoremView, refl, assume, trans) where

import Baysig.Expr
import Control.Monad

data Theorem = Thm [E] E 

data TheoremView = [E] :- E 
v (Thm es e) = es :- e

eq x y = EVar "==" $> x $> y
splitEq (EApp (EApp (EVar "==") x) y) = Just (x,y)
splitEq _ = Nothing

refl e = Just $ Thm [] (eq e e )
assume e = Just $ Thm [e] e
trans (Thm e1s e1) (Thm e2s e2) = do
      (lx, ly) <- splitEq e1
      (rx, ry) <- splitEq e2
      guard (ly==rx)
      return (Thm (e1s++e2s) (eq lx ry)) 