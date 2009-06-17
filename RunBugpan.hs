module Main where

import Parse
import Expr
import Database
import System.Environment
import Database
import System.Time
import ImpInterpret
import Traverse
import Transform
import Stages
import EvalM
import Numbers 
import Control.Monad


{-chainM :: Monad m => (s -> [a] -> m s)  -> [a] -> s -> m (s, [a])
chainM f [] s = return (s, [])
chainM f l@(x:xs) s = do (s', l') <- f s x 
                         chainM f l' s'-}

data RunState = RS { rstDecls :: [Declare], 
                     rstSess :: Maybe Session,
                     rstDt :: Maybe Double,
                     rstTmax :: Maybe Double
                   }


help = putStrLn $ unlines [
        "runbugpan [toptions] file\n\nOptions: ",
        "\t-n\t\tStart new session",
        "\t-c\t\tContinue last session",
        "\t-t {seconds}\tSet run length",
        "\t-d {seconds}\tSet timestep\n"

 ]

main = do
  args <- getArgs
  --print args 
  if null args 
    then help
    else dispatch (RS [] Nothing Nothing Nothing) args

dispatch rst ("-n":args) = do 
  s <- newSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args


dispatch rst ("-c":args) = do 
  s <- lastSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args

dispatch rst ("-d":dts:args) = dispatch (rst {rstDt = Just $ read dts}) args
dispatch rst ("-t":dts:args) = dispatch (rst {rstTmax = Just $ read dts}) args

dispatch rst (file:args) | head file /= '-' = do
  --print file
  ds <- fileDecls file []
  dispatch (rst {rstDecls = rstDecls rst ++ ds}) args

dispatch rst [] = go rst

go rs@(RS [] _ _ _) = return ()
go rs@(RS ds Nothing mdt mtmax) = do
  --mapM (putStrLn . ppDecl) ds
  let runTM = runTravM ds []
  let prg = snd . runTM $ transform
  let tmax = mtmax  `mplus` (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = mdt `mplus` (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  ress <- execInStages prg dt tmax return
  mapM_ print ress
  mapM_ showSig ress
  

go rs@(RS ds (Just sess) mdt mtmax) = do
  --get t0 from db
  tnow <- getClockTime
  let t0 = diffInS tnow $ tSessionStart sess
  let tmax = mtmax  `mplus` (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = mdt `mplus` (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  runOnce dt t0 tmax ds sess
  print "done running"
  return ()

showSig (nm,(SigV t1 t2 dt sf)) = do
  mapM (putStrLn . ppVal ) $ map sf [0..9]
  return ()
showSig _ = return ()

