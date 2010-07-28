{-# LANGUAGE TypeOperators #-}

module Baysig.Proof (Theorem, view, (:|-:),TheoremView, refl, assume) where

import Baysig.Expr

data Theorem = Thm [E] E 

data TheoremView = [E] :- E 
view (Thm es e) = es :- e

eq x y = EVar "==" $> x $> y

refl e = Thm [] (eq e e )
assume e = Thm [e] e
trans (Thm e1s e1) (Thm e2s e2) = Thm (e1s++e2s) 