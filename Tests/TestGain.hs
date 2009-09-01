{-# OPTIONS_GHC -i.. #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import System.Time
import Database
import Parse
import ImpInterpret
import EvalM hiding (ListT)
import Eval
import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans
import Control.Monad.State.Lazy
import HaskSyntaxUntyped
--import QueryUnsafe
import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
import QueryRun
import ValueIO
import Numbers
import Tests.Asserts
import PlotGnuplot

main = compileTest

loomAnal = inSessionNamed "5c17e342716081de800000110961a575" $ do
             ecV <- signals "ecVoltage" real
             tStart <- events "tStart" ()
             --plot [head ecV]
             liftIO . print $ meanF `sigStat`  ecV

snrBench = inSessionNamed "000" $ do
             ecV <- signalsDirect "ecVoltage" 
             --tStart <- (take 3) `fmap` events "tStart" ()
             --liftIO . print $ tStart
             --liftIO . print $ ecV
             --liftIO . print $ length ecV
             --plot . spreadOut . downSample 1000 . align tStart . normaliseBy stdDevF $ ecV
             liftIO . print $ sigStat minF ecV
ioBench = inTemporarySession $ do
              prog <- use "TestStore"
              run (prog`with` ["_tmax" =: dbl 10]) 0
              secs <- signals "secs" real
--              ecV <- signals "ecVoltage" real
              liftIO . print $ sigStat meanF (secs)

real = double


perfTest1 = inTemporarySession $ do
             intfire <- use "Intfire"
             run (intfire`with` ["_tmax" =: dbl 0.5]) 0
             sess <- get
             liftIO $ do ts <- sessionTypes sess
                         print ts
             vm <- signals "vm" real
             tStart <- events "tStart" ()
             synin <- events "rndSpike" ()
             --io $ print $ tStart
             --io $ print $ synin
             liftIO . print $ meanF `sigStat` vm
         
perfTest2 = inTemporarySession $ do
             intfire <- use "Intfire"
             run (intfire `with` ["_tmax" =: dbl 0.1]) 0
             vm <- signalsDirect "vm" 
             gcell <- signalsDirect "gcell" 
             gsyn <- signalsDirect "gsyn" 
             rndSpike <- events "rndSpike" ()
             liftIO $ print rndSpike
             liftIO $ gnuplotOnScreen $ vm

compileTest = inTemporarySession $ do
                intfire <- use "Intfire"
                prg <- compile intfire
                invoke prg
                vm <- signalsDirect "vm"
                rndSpike <- events "rndSpike" ()
                liftIO $ print rndSpike
                liftIO $ gnuplotOnScreen vm


  

unsafeMain = inTemporarySession $ do
  intfire <- use "Intfire"
  --putStrLn $ ppProg "IntFire" intfire
  --forM_ [0,10..100] $ \rate -> urun (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  run (intfire) 0
  --print gsy
  spikes <- events "spike" ()
  synin <- events "rndSpike" ()
  stim <- durations "inputRate" real
  vm <- signals "vm" real 
  gsyn <- signals "gsyn" real
  gcell <- signals "gcell" real
  let peakgsyn = peak gsyn 
      roi = fadeOut 20e-3 $ peak gsyn 
  --plotSig . head $ applyOverWith (/) gsyn roi
  --plotSig $ section gsynn (0, 20e-3, ())
  --plot (vm :+: 0 `tag` synin :+: (1e-10) `tag` spikes ) 
  liftIO $ print peakgsyn 
  --liftIO . print . area $  (flip (/) <$$> roi) `applyOver` gsyn

safeMain = inTemporarySession $ do
  intfire <- use "Intfire"
  forM_ [0,10..100] $ \rate -> run (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  --run intfire 0.1
  spike <- events "spike" ()
  stim  <- durations "inputRate" real
  --vm <- signals "vm"
  --plotSig (head vm)
  let q = freqDuring stim spike
  liftIO $ forM_ q (putStrLn . showDur)
  --io . print $ regressF `runStatsOn` q 
  return ()
