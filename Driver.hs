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
import Statement
import Parse

data DriverState = DS {
      dsSession :: Session,
      dsDispPullMV :: MVar (IO V),
      dsRunMV :: MVar (),
      dsProgram :: [Declare]
--      dsPrelude :: [Declare]
}
main = do 
  runningMv <- newEmptyMVar
  dispPullMv <- newEmptyMVar
  whenM (doesFileExist cmdFile) $ removeFile cmdFile

  forkOS (initGlScreen dispPullMv runningMv)

  sess <- lastSession "/home/tomn/sessions/"

  waitSecs 0.5

  let ds= DS sess dispPullMv runningMv  []
  forever $ loop ds

  return ()


--chainM :: Monad m => (a -> m a) -> [a] -> a -> m b
--chainM f (x:[]) = fx
--chainM f (x:xs) = 

cmdFile = "/tmp/program.bug"

loop ds@(DS (sess) dpmv rmv prg) = do
  ifM (not `fmap` fileExist cmdFile)
      (threadDelay 100000 >> loop ds)
      (do prg' <- fileDecls cmdFile [] -- read `fmap` readFile cmdFile

          let runTM = runTravM prg' (declsToEnv prelude)
          let prg = snd . runTM $ transform
          let tmax = (lookupDefn "_tmax" prg >>= vToDbl) `orJust` 1
          let dt = (lookupDefn "_dt" prg >>= vToDbl) `orJust` 0.001
          tnow <- getClockTime
          ress <- execInStages prg dt tmax $ postCompile dpmv rmv
          putStrLn $ "results for this trial: "++show ress
          addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
          removeFile cmdFile
          loop ds )

postCompile dispPullMv runningMv prg = do 
  let screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
--  let prgNoScreen = filter (noScreen screenVars) prg
--  let prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg
  if null screenVars
     then return prg
     else return $ prg++[
              GLParams dispPullMv runningMv
             ]

