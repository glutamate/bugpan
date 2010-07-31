{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.RunModule where

import Baysig.Expr
import Baysig.Eval

runModule :: Env -> [D] -> Either String Env
runModule env ds = 
   let constrs' = [(nm,ts) | DMkType _ _ cons <- ds, (nm, ts) <- cons]
       envwcon = env {constrs = constrs' ++ constrs env}
   in evalD envwcon ds

evalD env [] = return env
evalD env (DLet [pat] e:ds) = do
      v <- eval env e
      exts <- match pat v 
      evalD (extsEnv exts env) ds
evalD env (_:ds) = evalD env ds