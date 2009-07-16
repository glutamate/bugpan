{-# OPTIONS -fbang-patterns #-}

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
import QueryUnsafe
import Data.IORef

main = safeMain

safeMain = inTemporarySession $ do
  acqOnly <- use "AcqOnly"
  run (acqOnly `with` ["_tmax" =: dbl 1] ) 0
  return ()
