module Runner where

import Control.Concurrent
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
import HaskSyntaxUntyped
import Control.Monad.State.Strict
import System.Cmd
import Parse
--import Driver

data RunnerState = RS {
      lastTriggerTime :: Maybe ClockTime
}

type RunnerM = StateT RunnerState IO

cmdFile = "/tmp/program.bug"

use :: String -> [(String, E)] -> RunnerM ()
use fnm substs = 
    do -- liftIO . system $ "cp "++fnm++" "++cmdFile
       ds <-  liftIO $ fileDecls fnm substs
       liftIO $ writeFile cmdFile $ concatMap ppDecl ds
       tnow <- liftIO $ getClockTime
       put (RS $ Just tnow)
       return ()

wait :: Double -> RunnerM ()
wait s = do RS tm <- get
            case tm of
              Just t -> liftIO $ waitUntil t s
              Nothing -> liftIO $ waitSecs s 

ntimes :: Int -> RunnerM () -> RunnerM ()
ntimes 0 _ = return ()
ntimes n r = r >> ntimes (n-1) r