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
import Data.Maybe

dt = 0.001
tmax = 1
tsep = 2

main = do 
  runningMv <- newEmptyMVar
  dispPullMv <- newEmptyMVar

  sess <- newSession "/home/tomn/sessions/" 
  forkOS (initGlScreen dispPullMv runningMv)
  waitSecs 0.5

  forever $ loop dispPullMv runningMv sess

  return ()


loop dispPullMv runningMv sess = do
  prg' <- read `fmap` readFile "/home/tomn/test.bug"
  let runTM = runTravM prg' (declsToEnv prelude)
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude

  tnow <- getClockTime
  ress <- execInStages (complPrel++prg) dt tmax $ postCompile dispPullMv runningMv
  putStrLn $ "results for this trial: "++show ress
  addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
  return ()

  return ()

postCompile dispPullMv runningMv prg = do 
  let screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
--  let prgNoScreen = filter (noScreen screenVars) prg
--  let prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg
  if null screenVars
     then return prg
     else return $ prg++[
              GLParams dispPullMv runningMv
             ]