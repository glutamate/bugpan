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
import QueryTypes
import Control.Monad.Trans
import Control.Monad.State.Lazy
import HaskSyntaxUntyped

main = inTemporarySession $ do
  intfire <- use "Intfire"
  forM_ [0,10..100] $ \rate -> run (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  --run intfire 0.1
  spike <- events "spike"
  stim  <- durations "inputRate"
  --vm <- signals "vm"
  --plotSig (head vm)
  let q = spike`freqDuring` stim
  liftIO $ forM_ q (putStrLn . showDur)

  return ()

-- run many different inputrates
-- make gain plot
-- regression
-- post-spike signals like in fig 2

use :: MonadIO m => String -> m [Declare]
use fnm = liftIO $ fileDecls fnm []

with = flip makeSubs

run :: [Declare] -> Double -> StateT QState IO ()
run ds t0 = do
  sess <- getSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess
