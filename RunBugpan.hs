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
import TNUtils
import PrettyPrint
import HaskellBackend

{-chainM :: Monad m => (s -> [a] -> m s)  -> [a] -> s -> m (s, [a])
chainM f [] s = return (s, [])
chainM f l@(x:xs) s = do (s', l') <- f s x 
                         chainM f l' s'-}

data RunState = RS { rstDecls :: [Declare], 
                     rstSess :: Maybe Session,
                     rstDt :: Maybe Double,
                     rstTmax :: Maybe Double,
                     rstCompile :: Maybe String
                   }


help = putStrLn $ unlines [
        "runbugpan [options] file\n\nOptions: ",
        "\t-n\t\tStart new session",
        "\t-c\t\tContinue last session",
        "\t-t {seconds}\tSet run length",
        "\t-d {seconds}\tSet timestep",
        "\t-p\t\tParse file only",
        "\t-o {file}\tCompile to Haskell code\n"
 ]

main = do
  args <- getArgs
  --print args 
  if null args 
    then help
    else dispatch (RS [] Nothing Nothing Nothing Nothing) args

dispatch rst ("-n":args) = do 
  s <- newSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args


dispatch rst ("-c":args) = do 
  s <- lastSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args

dispatch rst ("-d":dts:args) = dispatch (rst {rstDt = Just $ read dts}) args
dispatch rst ("-t":dts:args) = dispatch (rst {rstTmax = Just $ read dts}) args

dispatch rst ("-o":fnm:args) = dispatch (rst {rstCompile = Just fnm}) args


dispatch rst ("-p":file:_) | head file /= '-' = do
  --print file
  ds <- fileDecls file []
  mapM_ print ds

dispatch rst (file:args) | head file /= '-' = do
  --print file
  ds <- fileDecls file []
  dispatch (rst {rstDecls = rstDecls rst ++ ds}) args

dispatch rst [] = go rst

go rs@(RS [] _ _ _ _) = return ()

go rs@(RS ds Nothing mdt mtmax Nothing) = do
  --mapM (putStrLn . ppDecl) ds
  let prg = getPrg rs
  let (tmax, dt) = (getTmax rs, getDt rs)
  ress <- execInStages prg dt tmax return
  mapM_ print ress
  mapM_ showSig ress
  

go rs@(RS ds (Just sess) mdt mtmax Nothing) = do
  --get t0 from db
  tnow <- getClockTime
  let t0 = diffInS tnow $ tSessionStart sess
  let (tmax, dt) = (getTmax rs, getDt rs)
  runOnce dt t0 tmax ds sess
  print "done running"
  return ()

go rs@(RS ds msess mdt mtmax (Just outNm)) = do
  let prg = getPrg rs
  let (tmax, dt) = (getTmax rs, getDt rs)
  compileToHask outNm dt tmax prg
  return ()

getPrg rs@(RS ds _ mdt mtmax _) 
    =   let runTM = runTravM ds [] in snd . runTM $ transform

getTmax rs@(RS ds _ mdt mtmax _) 
    = mtmax  `mplus` (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1

getDt rs@(RS ds _ mdt mtmax _) 
    =  mdt `mplus` (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001

showSig (nm,(SigV t1 t2 dt sf)) = do
  mapM (putStrLn . ppVal ) $ map sf [0..9]
  return ()
showSig _ = return ()

