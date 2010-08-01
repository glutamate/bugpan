{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.RunModule where

import Baysig.Expr
import Baysig.Eval

runModule :: Env -> [D] -> Either String Env
runModule env ds = 
   let constrs' = [(nm,ts) | DMkType _ _ cons <- ds, (nm, ts) <- cons]
       envwcon = env {constrs = constrs' ++ constrs env}
       dlets = [(reverse ps,e) | DLet ps e <- ds]
       f ([], ex) = []
       
       f ([p], ex) = fromRights (eval envwvals ex >>= match p)
       f (p:ps, ex) = f (ps, ELam p ex)
       envwvals = let nvals = concat $ (vals envwcon) : map f dlets
                  in envwcon {vals = nvals}
   in return envwvals

evalD env [] = return env
evalD env (DLet [pat] e:ds) = do
      v <- eval env e
      exts <- match pat v 
      evalD (extsEnv exts env) ds
evalD env (_:ds) = evalD env ds