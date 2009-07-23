{-# LANGUAGE BangPatterns #-}

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
--import QueryUnsafe
import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
import Math.Probably.PlotR

default (Double, Int)

{-spikes = uevents "spike" ()
synin = uevents "rndSpike" ()
stim = udurations "inputRate" double
vm = usignals "vm" double
gsyn = usignals "gsyn" double
 -}
loomAnal = inSessionNamed "5c17e342716081de800000110961a575" $ do
             ecV <- signals "ecVoltage" double
             tStart <- events "tStart" ()
             plot [head ecV]
             liftIO . print $ meanF `sigStat`  ecV

perfTest1 = inTemporarySession $ do
             intfire <- use "Intfire"
             run (intfire`with` ["_tmax" =: dbl 0.5]) 0
             sess <- get
             liftIO $ do ts <- sessionTypes sess
                         print ts
             vm <- signals "vm" double
             tStart <- events "tStart" ()
             synin <- events "rndSpike" ()
             io $ print $ tStart
             io $ print $ synin
             liftIO . print $ meanF `sigStat` vm
         
perfTest2 = inNewSession $ do
             intfire <- use "Intfire"
             run (intfire`with` ["_tmax" =: dbl 0.5]) 0
             vm <- signals "vm" double
             liftIO . print $ meanF `sigStat` vm

  

unsafeMain = inTemporarySession $ do
  intfire <- use "Intfire"
  --putStrLn $ ppProg "IntFire" intfire
  --forM_ [0,10..100] $ \rate -> urun (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  run (intfire) 0
  --print gsy
  spikes <- events "spike" ()
  synin <- events "rndSpike" ()
  stim <- durations "inputRate" double
  vm <- signals "vm" double
  gsyn <- signals "gsyn" double
  let peakgsyn = peak gsyn
      roi = fadeOut 20e-3 $ peak gsyn 
  --plotSig . head $ applyOverWith (/) gsyn roi
  --plotSig $ section gsynn (0, 20e-3, ())
  plot (vm :+: ((-0.06) `tag` synin) :+: ((-0.05) `tag` spikes) ) 
  liftIO $ print peakgsyn 
  liftIO . print . area $  (flip (/) <$$> roi) `applyOver` gsyn

main = perfTest1

safeMain = inTemporarySession $ do
  intfire <- use "Intfire"
  forM_ [0,10..100] $ \rate -> run (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  --run intfire 0.1
  spike <- events "spike" ()
  stim  <- durations "inputRate" double
  --vm <- signals "vm"
  --plotSig (head vm)
  let q = spike `freqDuring` stim
  liftIO $ forM_ q (putStrLn . showDur)
  io . print $ regressF `runStatsOn` q 
  return ()


io = liftIO
-- make gain plot
-- post-spike signals like in fig 2

-- http://en.wikipedia.org/wiki/Regression_analysis
{-regress :: (Tagged t) => [t (Double,Double)] -> (Double,Double) --tag of type num,num
regress vls = let xs = map (fst . getTag) vls
                  ys = map (snd . getTag) vls
                  xys = zip xs ys
                  mx = mean xs
                  my = mean ys
                  nume = sum $ map (\(x,y)->(x-mx)*(y-my)) xys
                  denom = sum $ map (square . (`sub` mx)) xs
                  slope = nume/denom
              in (slope,my-slope*mx)

square x = x*x
sub x y = x-y
-}


--from samfun
mean :: Fractional a =>  [a] -> a
mean = go 0 0
        where
            -- go ::  -> Int -> [Double] -> Double
            go s n []     = s / fromIntegral n
            go !s !n (x:xs) = go (s+x) (n+1) xs
