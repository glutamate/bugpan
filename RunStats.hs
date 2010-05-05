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
import Control.Monad.State.Lazy
import TNUtils
import Math.Probably.Sampler
import Math.Probably.Distribution
import Query (bugpanRootDir)
import ReactiveDistributions
import Query
import QueryTypes
import Data.List
import Data.Maybe
import Stats.Simulate

help = putStrLn $ unlines [
        "runstats simfakes {model file} {session base} {session count}\n\n"
        ]       

main = getArgs >>= dispatch

unConst (Const e) = e 

dispatch ("simfakes":filenm:sessBase:_) = do
  ds <- fileDecls filenm []
--  mapM print ds
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  fakeenv <- fmap (EM.env . head) $ runSamplerIO $ evalSim es ds
  --mapM print $ fakeenv
  saveEnv sessBase tmax dt $ fakeenv
  return ()

dispatch ("estimate":filenm:sessFiltr:_) = do
  ds <- fileDecls filenm []
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  return ()


dispatch _ = help

data RVar = RVar String T
          | InEvery String [RVar]

rvars :: [Declare] -> [RVar]
rvars = concatMap rvar 
   where rvar (Distribute p e) = [RVar (unsafePatToName p) (typeOfDist e)]
         rvar (Every p dure decls) = [InEvery (durExprToName dure) $ concatMap rvar decls]
         rvar _ = []

typeOfDist (App (App (Var "N") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "uniform") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "RandomSignal") _) _) = SignalT $ NumT (Just RealT)

durExprToName (Var nm) = nm
durExprToName (App (Var nm) _) = nm



