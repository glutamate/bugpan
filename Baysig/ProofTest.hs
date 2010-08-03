module Main where

import Baysig.Expr
import Baysig.Proof.Core
import Baysig.Proof.Equal
import Baysig.RunModule

showE (EApp (EApp (EVar "==") x) y) = show x ++ " == " ++ show y
showE e = show e

showThm thm 
   = let (asm,e) = v thm
     in unlines (map showE asm ++ 
                ["---------------", 
                 showE e])

printProof thm = putStrLn $ showThm thm

main = do (es,t) <- inEnv "Nats.bug" $ \env -> do
                 er2 <- evalRule env (EVar "plus"
                                       $> EVar "three"
                                       $> EVar "three")
                 return (env, er2)
          mapM_ print $ fst es
          putStrLn ""
          printProof t