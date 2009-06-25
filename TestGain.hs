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

main = do
  sess<- newSession "/home/tomn/sessions/"
  ds <- fileDecls "Intfire.bug" []
  tnow <- getClockTime
  let t0 = diffInS tnow $ tSessionStart sess
  let tmax = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  runOnce dt t0 tmax ds sess
  runOnce dt (t0+tmax+0.5) tmax ds sess

  q <- inLastSession $ do
         spike <- events "spike"
         stim  <- durations "inputRate"
         return $ zip stim (spike`freqDuring` stim)
  print q
  deleteSession sess
