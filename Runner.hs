module Runner where

import Control.Concurrent
import Compiler
import Expr
import ImpInterpret
import Control.Monad
import Database
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
import System.Random
--import Driver
import TNUtils
import PrettyPrint

data RunnerState = RS {
      lastTriggerTime :: Maybe ClockTime,
      cmdFile :: String,
      shArgs :: [String]
}

type RunnerM = StateT RunnerState IO

type Range a = Double -> a

--determines :: String -> [(String, Range V)] -> RunnerM ()

--runGoals :: [(Int, RunnerM ())] -> RunnerM ()

setTriggerTimeToNow :: RunnerM ()
setTriggerTimeToNow = do tnow <- liftIO $ getClockTime
                         RS _ cmd args <- get
                         put (RS (Just tnow) cmd args)

go :: [String] -> RunnerM a -> IO a
go args ra = do 
	sess <- lastSession "/var/bugpan/sessions/"
	fst `fmap` runStateT ra (RS Nothing (oneTrailingSlash(baseDir sess)++"/program.bug") args) 

--cmdFile = "/tmp/program.bug"

use :: String -> [(String, E)] -> RunnerM ()
use fnm substs = 
    do -- liftIO . system $ "cp "++fnm++" "++cmdFile
       ds <-  liftIO $ fileDecls fnm substs
       cmdFl <- cmdFile `fmap` get
       args <- shArgs `fmap` get
       --liftIO $ print args
       if "-nodaq" `elem` args
          then liftIO $ do --print "removing daq"
                           writeFile cmdFl $ show (noDaqTrans ds)
          else liftIO $ writeFile cmdFl $ show ds -- ppProg "RunProgram" ds
       setTriggerTimeToNow
       return ()

wait :: Double -> RunnerM ()
wait s = do RS tm cf _ <- get
            case tm of
              Just t -> liftIO $ waitUntil t s
              Nothing -> liftIO $ waitSecs s 

uniform :: (Random a, Ord a) => a -> a-> RunnerM a 
uniform lo hi = liftIO $ randomRIO (min lo hi, max lo hi) 

oneOf :: [a] -> RunnerM a
oneOf xs = do idx <- uniform 0 (length xs -1)
              return $ xs !! idx

trace :: Show a => String -> a -> RunnerM ()
trace nm v = liftIO . putStrLn $ nm++" "++show v

--wholeDuration :: ToVal a -> String -> a -> RunnerM ()

ntimes :: Int -> RunnerM () -> RunnerM ()
ntimes 0 _ = return ()
ntimes n r = r >> ntimes (n-1) r

noDaqTrans ds = let daqVars = [ nm | ReadSource nm ("adc", _) <- ds ]
                    hasDaqVar (ReadSource nm _) = nm `elem` daqVars
                    hasDaqVar (SinkConnect (Var nm) _) = nm `elem` daqVars
                    hasDaqVar _ = False
                in filter (not . hasDaqVar) ds
