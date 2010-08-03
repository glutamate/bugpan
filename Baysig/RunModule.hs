{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.RunModule where

import Baysig.Expr
import Baysig.Eval
import Baysig.Transform
import Baysig.Syntax.Parser

getConstrs ds = [(nm,ts) | DMkType _ _ cons <- ds, (nm, ts) <- cons]

runModule :: Env -> [D] -> Either String Env
runModule env ds = 
   let constrs' = getConstrs ds
       envwcon = env {constrs = constrs' ++ constrs env}
       dlets = [(reverse ps,e) | DLet ps e <- ds]
       f ([], ex) = []
       
       f ([p], ex) = fromRights (eval envwvals ex >>= matchPV p)
       f (p:ps, ex) = f (ps, ELam p ex)
       envwvals = let nvals = concat $ (vals envwcon) : map f dlets
                  in envwcon {vals = nvals}
   in return envwvals

inEnv :: String -> 
         (([(String,E)],[(String,[T])]) -> Either String a) -> 
         IO a
inEnv fnm f = do
   testBug <- readFile fnm
   ds <- case parseDs testBug of
           Left err -> fail $ "parse error in "++fnm++": "++ err 
           Right ds -> return$ removeTopLevelPatterns ds 
   --mapM_ print $ ds
   --mapM_ print $ removeTopLevelPatterns ds
   let defs = [(nm,e) | DLet [PVar nm] e <- ds]
   let constrs = getConstrs ds
   case f (defs,constrs) of
        Left err -> fail $ "withEnv: supplied function fails: "++ err 
        Right x -> return x
