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

main = detectSpikes "06ee"

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

detectSpikes snm = inApproxSession snm $ do
                 running <- durations "loom" ()
                 unlessM (exists "normV")
                         (do ecV <- signalsDirect "ecVoltage" 
                             liftIO $ putStrLn "recalculatino normV"
                             storeAs "normV" $ subMeanNormSD ecV
                             return () )
                 normV <- signalsDirect "normV"
                 tStart <-  events "tStart" ()
                 let thresh = ((atMost (-13)) . (*0.70)) <$$> sigStat minF normV

                 unlessM (exists "spikes") $ do                                                              
                                       storeAsOvwrt "spikes" $ crossesDown thresh normV
                                       return ()
                 spikes <- events "spikes" ()
                 ask $ plotManyBy running $ normV :+: (-13) `tagd` spikes :+: thresh


                 --ask $ plotManyBy running $ normV :+: (-13) `tagd` spikes :+: thresh


loomAnal snm = inApproxSession snm $ do                
                 running <- durations "running" ()
                 unlessM (exists "normV")
                         (do ecV <- signalsDirect "ecVoltage" 
                             liftIO $ putStrLn "recalculatino normV"
                             storeAs "normV" $ subMeanNormSD ecV
                             return () )
                 normV <- signalsDirect "normV"
                 tStart <-  events "tStart" ()
                 let thresh = ((atMost (-13)) . (*0.70)) <$$> sigStat minF normV
                 unlessM (exists "spikes") $ do                                                              
                                       storeAs "spikes" $ crossesDown thresh normV
                                       return ()
                 spikes <- events "spikes" ()
                 loom <- (take 150) `fmap` durations "DisplacedLoom" ()
                 let runStart = tagTime loom

                 --ask $ plotManyBy running $ normV :+: (-13) `tagd` spikes :+: thresh
                 let repetition =  whollyCycleLabel [1..30] loom
                 --liftIO . print2 "loom " $  loom
                -- liftIO . print2 "repetition " $  repetition 
                 --liftIO . print2 "runStart " $  runStart
                 liftIO . print2 "repetition " $  groupBy repetition runStart `groupStats` meanSDF
