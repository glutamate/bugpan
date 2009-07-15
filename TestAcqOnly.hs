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

