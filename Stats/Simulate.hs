module Stats.Simulate where

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

saveEnv :: String -> Double ->  Double -> [(String,V)] -> IO ()
saveEnv _ tmax dt [] = return ()
saveEnv sessBase tmax dt ((nm, v):rest) | "session" `isPrefixOf` nm = do
  let sessNm = sessBase++drop 7 nm
  deleteSessionIfExists sessNm
  inApproxSession ("new:"++sessBase++drop 7 nm) $ saveSess tmax dt v
  return ()
saveEnv sB tmax dt (_:rest) = saveEnv sB tmax dt rest

saveSess :: Double -> Double -> V -> QueryM ()
saveSess tmax dt (ListV vs)  = forM_ vs $ saveSess tmax dt
saveSess tmax dt (PairV (StringV durnm) (ListV vs)) | "running" `isPrefixOf` durnm = do
  t0 <- lastTStop `fmap` get
  forM_ vs $ saveVal t0 dt tmax
  modify (\s -> s {lastTStop = t0+tmax+1})

makeSavable :: Double -> V -> V
makeSavable tmax v@(SigV t1 t2 dt sf) =  v
makeSavable tmax v | isEvents v = v
                   | isEpochs v =  v
                   | otherwise = ListV [PairV (PairV 0 (pack tmax)) v]

saveVal t0 dt tmax (PairV (StringV nm) v) = do
  sess <- getSession
  liftIO $ saveInSession sess nm t0 dt $ makeSavable tmax v
saveVal _ _ _ v = error $ "unknown val: "++show v
 
evalSim :: EvalS -> [Declare] -> Sampler EvalS
evalSim env [] = return env
evalSim env ((Distribute (PatVar nm _) dist):rest) = do
   newVal <- drawFake env dist
   evalSim (extEnv (nm,newVal) env) rest
evalSim env ((Let (PatVar nm _) e):rest) = do
   let newVal = unEvalM $ eval env e
   evalSim (extEnv (nm,newVal) env) rest
evalSim es ((Every (PatVar nm _) (App (Var durnm) counte) decls):rest) = do
   let NumV (NInt n) = unEvalM $ eval es counte
   let currentHead = fst $ head $ env es
   retenvs <- forM [1..n] $ const $ do
         newe <- evalSim es decls --incomplete. how to update envs?
         return $ takeWhile ((/=currentHead) . fst) $ EM.env newe
   let extNms :: [String]
       extNms = map ((durnm++) . show) [0..(n-1)]
       fixEnv :: [(String,V)] -> V
       fixEnv nmvals = ListV $ map (\(nm, val) -> PairV (StringV nm) val) nmvals 
       extion :: [(String,V)]
       extion = zip extNms $ map fixEnv retenvs
   evalSim (extsEnv extion es) rest

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

drawFake env  (App (App (Var "RandomSignal") me) sde) = do
  let m = unEvalM $ eval env me
  let sd = unEvalM $ eval env sde
  u <- sampler $ RandomSignal (unsafeReify m) (unsafeReify sd)
  return $ pack u
