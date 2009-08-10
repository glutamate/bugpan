module Main where

import Control.Concurrent
import OpenGL
import Compiler
import Expr
import ImpInterpret
import Control.Monad
import Database
--import BugPrelude 
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
import GHC.Handle
import System.IO
import System.Environment
import TNUtils
import Control.Monad.Trans

data DriverState = DS {
      dsSession :: Session,
      dsDispPullMV :: MVar (IO V),
      dsRunMV :: MVar (),
      dsProgram :: [Declare]
--      dsPrelude :: [Declare]
}
main = do 
  args <- getArgs
  runningMv <- newEmptyMVar
  dispPullMv <- newEmptyMVar
  sess <- lastSession "/home/tomn/sessions/"
  print sess
  let redirect = not $ "-nr" `elem` args
  when redirect $ do
	  let outFileName = oneTrailingSlash(baseDir sess)++"driver_output.txt"
	  putStrLn $"redirecting stdout to "++outFileName
	  hout<- openFile outFileName AppendMode
	  hDuplicateTo hout stdout

  let ds= DS sess dispPullMv runningMv  []
  whenM (doesFileExist $ cmdFile ds) $ removeFile (cmdFile ds)


  forkOS (initGlScreen (not $ "-w" `elem` args) dispPullMv runningMv)
  waitSecs 0.5

  catchForever $ (loop ds >> hFlush stdout)

  return ()


--chainM :: Monad m => (a -> m a) -> [a] -> a -> m b
--chainM f (x:[]) = fx
--chainM f (x:xs) = 

cmdFile (DS (sess) dpmv rmv prg)= oneTrailingSlash(baseDir sess)++"/program.bug"

catchForever l = forever $ catch l (\e-> putStrLn $ "error in main loop: "++show e)

loop ds@(DS (sess) dpmv rmv prg) = do
  ifM (not `fmap` fileExist (cmdFile ds))
      (threadDelay 100000)
      (do prg' <- liftIO (read `fmap` readFile (cmdFile ds))

          let runTM = runTravM prg' []
          let prg = snd . runTM $ transform
          let tmax = (lookupDefn "_tmax" prg >>= vToDbl) `orJust` 1
          let dt = (lookupDefn "_dt" prg >>= vToDbl) `orJust` 0.001
          tnow <- getClockTime
          ress <- execInStages prg dt tmax $ postCompile dpmv rmv
          putStrLn $ "results for this trial: "++show ress
          addRunToSession prg (diffInS tnow (tSessionStart sess)) tmax dt ress sess
          removeFile $ cmdFile ds)

postCompile dispPullMv runningMv prg = do 
  let screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
--  let prgNoScreen = filter (noScreen screenVars) prg
--  let prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg
  if null screenVars
     then return prg
     else return $ prg++[
              GLParams dispPullMv runningMv
             ]

