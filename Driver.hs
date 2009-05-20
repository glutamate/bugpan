module Main where

import Control.Concurrent
import OpenGL
import Compiler
import Expr
import ImpInterpret
import Control.Monad
import Database
import BugPrelude 
import Traverse
import Stages
import Transform
import System.Time


dt = 0.001
tmax = 1
tsep = 2

main = do 
  runningMv <- newEmptyMVar
  dispPullMv <- newEmptyMVar

  sess <- newSession "/home/tomn/sessions/" 
  initGlScreen dispPullMv runningMv

  forever $ loop dispPullMv runningMv sess

  return ()


loop dispPullMv runningMv sess = do
  prg' <- read `fmap` readFile "/home/tomn/test.bug"
  let runTM = runTravM prg' (declsToEnv prelude)
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude

  --here add triggers etc to screen signal.

  tnow <- getClockTime
  ress <- execInStages (complPrel++prg) dt tmax
  putStrLn $ "results for this trial: "++show ress
  addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
  return ()

  return ()