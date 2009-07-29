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

data RunnerState = RS {
      lastTriggerTime :: Maybe ClockTime,
      cmdFile :: String
}

type RunnerM = StateT RunnerState IO

go :: RunnerM a -> IO a
go ra = do 
	sess <- lastSession "/home/tomn/sessions/"
	fst `fmap` runStateT ra (RS Nothing $ oneTrailingSlash(baseDir sess)++"/program.bug") 

--cmdFile = "/tmp/program.bug"

use :: String -> [(String, E)] -> RunnerM ()
use fnm substs = 
    do -- liftIO . system $ "cp "++fnm++" "++cmdFile
       ds <-  liftIO $ fileDecls fnm substs
       cmdFl <- cmdFile `fmap` get
       liftIO $ writeFile cmdFl $ ppProg "RunProgram" ds
       tnow <- liftIO $ getClockTime
       put (RS (Just tnow) cmdFl)
       return ()

wait :: Double -> RunnerM ()
wait s = do RS tm cf <- get
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
