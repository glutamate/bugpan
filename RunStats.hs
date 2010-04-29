module Main where

import Parse
import Expr
import System.Environment
import Database
import Traverse
import EvalM as EM
import Eval
import Numbers 
import Control.Monad
import TNUtils
import Math.Probably.Sampler
import Query (bugpanRootDir)

help = putStrLn $ unlines [
        "runstats simfakes {model file}\n\n"
        ]       

main = getArgs >>= dispatch

dispatch ("simfakes":filenm:_) = do
  ds <- fileDecls filenm []
  mapM print ds
  fakeenv <- fmap head $ runSamplerIO $ evalSim emptyEvalS ds
  mapM print $ EM.env fakeenv
  return ()

dispatch _ = help

evalSim :: EvalS -> [Declare] -> Sampler EvalS
evalSim env [] = return env
evalSim env ((Distribute (PatVar nm _) dist):rest) = do
   newVal <- drawFake env dist
   evalSim (extEnv (nm,newVal) env) rest
evalSim env ((Let (PatVar nm _) e):rest) = do
   let newVal = unEvalM $ eval env e
   evalSim (extEnv (nm,newVal) env) rest

evalSim env ((Every (PatVar nm _) (App (Var durnm) counte) decls):rest) = do
   let NumV (NInt n) = unEvalM $ eval env counte
   forM [1..n] $ const $ do
               evalSim env decls --incomplete. how to update envs?
   evalSim env rest
      



evalSim env (_:rest) = return env

drawFake :: EvalS -> E -> Sampler V
drawFake env (App (Var "unknown") (Const v)) = return v
drawFake env  (App (App (Var "uniform") loe) hie) = do
  let lo = unEvalM $ eval env loe
  let hi = unEvalM $ eval env hie
  u <- uniform lo hi
  return u
drawFake env  (App (App (Var "N") me) sde) = do
  let NumV m = unEvalM $ eval env me
  let NumV sd = unEvalM $ eval env sde
  u <- gauss m sd
  return $ NumV u

rvars :: [Declare] -> [(String, Int)]
rvars = concatMap rvar 
   where rvar (Distribute p e) = [(unsafePatToName p, 0)]
         rvar _ = []


