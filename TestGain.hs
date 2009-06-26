module Main where

import System.Time
import Database
import Parse
import ImpInterpret
import EvalM hiding (ListT)
import Eval
import Expr
import Stages
import Query
import Control.Monad.Trans
import Control.Monad.State.Lazy
import HaskSyntaxUntyped

main = inTemporarySession $ do
  intfire <- use "Intfire"
  --forM_ [0,10..100] $ \rate -> run (intfire `with` ["inputRate" =: dbl rate] ) 0.5
  run intfire 0.1
  spike <- events "spike"
  stim  <- durations "inputRate"
  vm <- signals "vm"
  plotSig (head vm)
  let q = zip stim (spike`freqDuring` stim)
  liftIO $ print q

  return ()

-- run many different inputrates
-- make gain plot
-- regression
-- post-spike signals like in fig 2

use :: MonadIO m => String -> m [Declare]
use fnm = liftIO $ fileDecls fnm []

with = flip makeSubs

run :: [Declare] -> Double -> StateT QState IO ()
run ds twait = do
  tmax <- sessionTmax
  sess <- getSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt (tmax+twait) trun ds sess
