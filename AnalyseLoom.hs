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
import QueryPlots

main = calcNormV "6f"

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

calcNormV snm =  inApproxSession snm $ do
                 running <- durations "Loom" ()
                 --ask running
                 storeAsOvwrt "repetition" $ labelMagically 60 30 running
                 rep <- durations "repetition" (1::Int)
                 unlessM (exists "normV")
                         (do ecV <- signalsDirect "ecVoltage" 
                             liftIO $ putStrLn "recalculatino normV"
                             storeAs "normV" $ subMeanNormSD (during rep ecV)
                             return () ) 

{-detectSpikes snm = inApproxSession snm $ do
                 running <- durations "Loom" ()
                 --rep <- durations "repetition" (1::Int)-}
                 normV <- signalsDirect "normV"
                 --tStart <-  events "tStart" ()
                 let thresh = durd (-9) --((atMost (-10)) . (*0.70)) <$$> sigStat minF (dropSecs 2 normV)

                 --unlessM (exists "spikes") $ do                                                              
                 storeAsOvwrt "spikes" $ crossesDown thresh normV
                                       --return ()
                 spikes <- events "spikes" () 

                 ask $ plotManyBy running $ normV :+: 
                                            (-13) `tagd` spikes :+: 
                                            thresh :+: rep
                 liftIO . mapM_ print $  groupBy rep (snd <$$> freqDuring rep spikes)  `groupStats` meanSDF


                 --ask $ plotManyBy running $ normV :+: (-13) `tagd` spikes :+: thresh


loomAnal snm = inApproxSession snm $ do                
                 {-running <- durations "running" ()
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
                 --let repetition =  whollyCycleLabel [1..30] loom
                 repetition <- durations "repetition" (1::Int)
                 --liftIO . print2 "loom " $  loom
                -- liftIO . print2 "repetition " $  repetition 
                 --liftIO . print2 "runStart " $  runStart -}
                 rep <- durations "repetition" (1::Int)
                 spikes <- events "spikes" ()


                 liftIO . print2 "repetition " $  groupBy rep (snd <$$> freqDuring rep spikes)  `groupStats` meanSDF
