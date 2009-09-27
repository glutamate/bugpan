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
import System.Environment

main = spikeDetectIO

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []


spikeDetectIO = do
  (snm:overDurNm:_) <-getArgs
  inApproxSession snm $ do
                  openReplies
                  initUserInput
                  overDur <- unitDurations overDurNm
                  normV <- signalsDirect "normV"
                  spikeDetect [overDur] normV


spikeDetect overs normV = do
  let over = head overs
  io $ putStrLn $ "currently considering "++show (length over)++" durations"
  userChoice [('s', "show normV", 
                  do --normV <- signalsDirect "normV"
                      ask $ plotManyBy over $ normV
                      spikeDetect overs normV),
              ('u', "undo restrict",
                   case overs of 
                     [o] -> spikeDetect overs normV
                     o:os->spikeDetect os normV),
              ('r', "restrict trials", 
                   do ndrop <- userValue "number to drop"
                      ntake <- userValue "number to take"
                      spikeDetect ((take ntake $ drop ndrop over):overs) normV),
              ('f' , "fixed threshold", 
                   do thr <- userValue "threshold"
                      let thresh = durd (thr)
                      let spikes = crossesDown thresh normV
                      ask $ plotManyBy over $ normV :+: thresh :+: thr `tagd` spikes
                      whenM (userConfirm "store spikes")
                            (do storeAsOvwrt "spikes" spikes
                                return ())
                      spikeDetect overs normV)]  
                                           

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

                 ask $ plotManyBy rep $ normV :+: 
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
