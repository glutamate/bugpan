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
  let tmax = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  runOnce dt 0 tmax ds sess
  runOnce dt (tmax+0.5) tmax ds sess

  q <- inLastSession $ do
         spike <- events "spike"
         stim  <- durations "inputRate"
         vm <- signals "vm"
         plotSig (head vm)
         return $ zip stim (spike`freqDuring` stim)
  print q
  deleteSession sess
