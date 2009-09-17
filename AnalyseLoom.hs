{-# LANGUAGE BangPatterns #-}

module Main where

--import System.Time
--import Database
--import Parse
--import EvalM hiding (ListT)
--import Eval
--import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans
--import Control.Monad.State.Lazy
--import HaskSyntaxUntyped
--import QueryUnsafe
--import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
--import ValueIO
--import Numbers
import TNUtils
import PlotGnuplot

main = loomAnal "b20d"

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

loomAnal snm = inApproxSession snm $ do                
                 running <- durations "running" ()
                 unlessM (exists "normV")
                         (do ecV <- signalsDirect "ecVoltage" 
                             liftIO $ putStrLn "recalculatino normV"
                             storeAs "normV" $ subMeanNormSD ecV
                             return () )
                 normV <- signalsDirect "normV"
                 let thresh = ((atMost (-13)) . (*0.70)) <$$> sigStat minF normV
                 spikes <- storeAs "spikes" $ crossesDown thresh normV
                 ask $ plotManyBy running $ normV :+: (-13) `tagd` spikes :+: thresh
