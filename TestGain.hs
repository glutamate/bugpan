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
import Math.Probably.PlotR
import QueryRun

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

snrBench = inSessionNamed "72cf2d2c868a81de800000110961a575" $ do
             ecV <- signals "ecVoltage" double
             liftIO . print $ sigStat minF (take 10 ecV)
ioBench = inTemporarySession $ do
              prog <- use "TestStore"
              run (prog`with` ["_tmax" =: dbl 100]) 0
              secs <- signals "secs" double
--              ecV <- signals "ecVoltage" double
              liftIO . print $ sigStat minF (secs)

ioTest = inTemporarySession $ do
           prog <- use "TestStore"
           run prog 0
           run prog 5
           run prog 10

           secs <- signals "secs" double
           anEvent <- events "anEvent" ()
           aNumEvent <- events "aNumEvent" double
           aStringDur <- durations "aStringDur" ""
           aPairDur <- durations "aPairDur" (double, ())

           assertTagsBetween "secs stdDev" (0.28,0.30) $ sigStat stdDevF secs
           assertEqual "#secs" 3 $ length secs
           assertEqual "#events" 3 $ length anEvent
           assertEqual "#anumevent" 6 $ length aNumEvent
           assertEqual "#strdur" 3 $ length aStringDur
           assertEqual "#pairdur" 6 $ length aPairDur

           assertEvTimesBtw "anEvent time (2/3 fail)" (0.29,0.31) $ anEvent
           assertTagsEqual "anEvent tag" () anEvent
           assertTagsBetween "aNumEvent tag" (4.5, 6.6) aNumEvent

           assertTagsEqual "aStringDur val" "foo"  aStringDur

           assertTagsEqual "pairdur unit tag" () (snd <$$> aPairDur)
           assertTagsBetween "pairdur num tag" (0.5,2.5) (fst <$$> aPairDur)

           sigStartBetween "secs start" (4.9,5.1) $ secs !!1
           sigEndBetween "secs stop" (5.9,6.1) $ secs !!1
           sigDtBetween "secs dt" (0.0001, 0.01) $ secs !!1

sigStartBetween str rng (Signal t1 _ _ _) =
    assertBetween str rng t1
sigEndBetween str rng (Signal _ t2 _ _) =
    assertBetween str rng t2
sigDtBetween str rng (Signal t1 _ dt _) =
    assertBetween str rng dt

assertBetween str (lo, hi) x  = 
    if x>lo && x < hi
       then liftIO . putStrLn $ str++ " ok"
       else liftIO . putStrLn $ str ++" FAIL: not "++show lo++"<"++show x++"<"++show hi
assertEqual str x y = 
    if x==y 
       then liftIO . putStrLn $ str++ " ok"
       else liftIO . putStrLn $ str ++" FAIL: not "++show x++"=="++show y

assertTagsBetween str rng tgs = do
  let tags = map getTag tgs 
  forM tags $ \val -> assertBetween str rng val
assertTagsEqual str x tgs = do
  let tags = map getTag tgs 
  forM tags $ \val -> assertEqual str x val

assertEvTimesBtw str rng tgs = do
  let tms = map fst tgs 
  forM tms $ \tm -> assertBetween str rng tm
              

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
             run (intfire `with` ["_tmax" =: dbl 0.5]) 0
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
  gcell <- signals "gcell" double
  let peakgsyn = peak gsyn 
      roi = fadeOut 20e-3 $ peak gsyn 
  --plotSig . head $ applyOverWith (/) gsyn roi
  --plotSig $ section gsynn (0, 20e-3, ())
  plot (vm :+: 0 `tag` synin :+: (1e-10) `tag` spikes ) 
  liftIO $ print peakgsyn 
  --liftIO . print . area $  (flip (/) <$$> roi) `applyOver` gsyn

main = ioTest

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
