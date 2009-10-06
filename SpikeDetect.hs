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
import Data.List hiding (groupBy)
import Data.Ord
import System.Environment

main = spikeDetectIO

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

autoSpikes sigNm = do
  sigs <- take 10 `fmap` signalsDirect sigNm 
  let sd = stdDevF `sigStat` sigs
  let sigu = upsample 5 sigs
  let putatives = crossesDown (((*5) . negate) <$$> sd) sigu `catevents` 
                  crossesUp ((*5) <$$> sd) sigu
  let waveforms = limitSigs' (-0.001) 0.001 $ around putatives $ sigu
  let alignWaveforms = alignBy (centreOfMass . (square <$$>) . (limitSigs (-0.0005) 0.0005)) waveforms
  let w1 = take 1 waveforms
  ask $ plot $ take 1 $ limitSigs' 0 0.002 sigu
  --ask $ plot $ take 10 alignWaveforms
  return ()

spikeDetectIO = do 
  (snm:overDurNm:_) <-getArgs
  inApproxSession snm $ do
                  openReplies
                  initUserInput
                  plotSize 490 329
                  overDur <- unitDurations overDurNm
                  autoSpikes "normV"
                  --normV <- signalsDirect "normV"
                  --spikeDetect [overDur] normV []


spikeDetect overs normV spks = do
  let over = head overs
  io $ putStrLn $ "currently considering "++show (length over)++" durations, "++show (length spks)++" spikes"
  userChoice [('s', "show normV and existing spikes", 
                  do --normV <- signalsDirect "normV"
                      spikes <- events "spikes" () 
                      ask $ plotManyBy over $ normV :+: ("stored spikes", (-10) `tagd` spikes) :+: 
                              ("New spikes", (-12) `tagd` spks)
                      spikeDetect overs normV spks),
              ('u', "undo restrict",
                   case overs of 
                     [o] -> spikeDetect overs normV spks
                     o:os->spikeDetect os normV spks),
              ('c', "clear detected spikes",
                    spikeDetect overs normV []),
              ('q', "quit",
                  return ()),
              ('d', "delete stored spikes",
                  do deleteValue "spikes"
                     spikeDetect overs normV spks),
              ('v', "save detected spikes",
                    do storeAsOvwrt "spikes" $ sortBy (comparing fst) spks
                       spikeDetect overs normV spks),
              ('r', "restrict trials", 
                   do ndrop <- userValue "number to drop"
                      ntake <- userValue "number to take"
                      spikeDetect ((take ntake $ drop ndrop over):overs) normV spks),
              ('f' , "fixed threshold", 
                   do thr <- userValue "threshold"
                      let thresh = durd (thr)
                      let spikes = during over $ crossesDown thresh normV
                      ask $ plotManyBy over $ normV :+: thresh :+: thr `tagd` spikes
                      ifM (userConfirm "accept spikes")
                          (spikeDetect overs normV (spks++spikes))
                          (spikeDetect overs normV spks))]  
                                           

