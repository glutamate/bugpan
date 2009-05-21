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
import EvalM
import System.Posix.Files
import System.Directory

data DriverState = DS {
      dsSession :: Maybe (Session),
      dsDispPullMV :: MVar (IO V),
      dsRunMV :: MVar (),
      dsDt :: Double,
      dsTmax :: Double,
      dsProgram :: [Declare]
--      dsPrelude :: [Declare]
}
main = do 
  runningMv <- newEmptyMVar
  dispPullMv <- newEmptyMVar

  forkOS (initGlScreen dispPullMv runningMv)
  waitSecs 0.5
  let ds= DS Nothing dispPullMv runningMv 0.001 0.1 []
  forever $ loop ds

  return ()


--chainM :: Monad m => (a -> m a) -> [a] -> a -> m b
--chainM f (x:[]) = fx
--chainM f (x:xs) = 

cmdFile = "/home/tomn/test.bug"

loop  ds = do
  ifM (not `fmap` fileExist cmdFile)
      (threadDelay 100000 >> loop ds)
      (do cmd <- read `fmap` readFile cmdFile
          newds <- dispatch ds cmd
          removeFile cmdFile
          loop newds )


dispatch (DS msess dpmv rmv dt _ _) (Prepare prg' tmax) = do
  let runTM = runTravM prg' (declsToEnv prelude)
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude
  return $ DS msess dpmv rmv dt  tmax (complPrel++prg)

dispatch ds@(DS (Just sess) dpmv rmv dt tmax prg) Go = do
  tnow <- getClockTime
  ress <- execInStages prg dt tmax $ postCompile dpmv rmv
  putStrLn $ "results for this trial: "++show ress
  addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
  return ds

dispatch ds NewSession = do
  sess <- newSession "/home/tomn/sessions/" 
  return $ ds {dsSession = Just sess }

postCompile dispPullMv runningMv prg = do 
  let screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
--  let prgNoScreen = filter (noScreen screenVars) prg
--  let prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg
  if null screenVars
     then return prg
     else return $ prg++[
              GLParams dispPullMv runningMv
             ]


data DriverCmd = Prepare [Declare] Double 
               | Go 
               | NewSession 
               | UseSession String
                 deriving (Show, Eq, Read)

