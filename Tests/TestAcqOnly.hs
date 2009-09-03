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
import Data.IORef
import QueryRun
import Math.Probably.FoldingStats
import TNUtils
import QueryUtils


main = safeMain

safeMain = inTemporarySession $ do
  acqOnly <- use "AcqOnly"
  run (acqOnly `with` ["_tmax" =: dbl 1] ) 0
  ecV <- signalsDirect "ecVoltage"
  liftIO . print2 "min" $ sigStat minF ecV
  liftIO . print2 "max" $ sigStat maxF ecV
  liftIO . print2 "mean,SD" $ sigStat meanSDF ecV

  return ()
