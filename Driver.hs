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
  initGlScreen dispPullMv runningMv

  forever $ loop dispPullMv runningMv sess

  return ()


loop dispPullMv runningMv sess = do
  prg' <- read `fmap` readFile "/home/tomn/test.bug"
  let runTM = runTravM prg' (declsToEnv prelude)
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude

  --here add opengl triggers etc if screen signal.

  tnow <- getClockTime
  ress <- execInStages (complPrel++prg) dt tmax $ postCompile dispPullMv runningMv
  putStrLn $ "results for this trial: "++show ress
  addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
  return ()

  return ()

postCompile dispPullMv runningMv prg = do 
  let screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
  let prgNoScreen = filter (noScreen screenVars) prg
  let prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg

  return $prg++[
              Trigger . const $ do putMVar runningMv (), -- will have to handle errors in dispPull if run on first iter
              RunAfterDone . const $takeMVar runningMv
 --dont have env, can't build dispPull!!
             ]