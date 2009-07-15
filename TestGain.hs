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

spikes = uevents "spike" ()
stim = udurations "inputRate" double
gsyn = usignals "gsyn" double

unsafeMain = do
  openNewSession
  intfire <- use "Intfire"
  --putStrLn $ ppProg "IntFire" intfire
  --forM_ [0,10..100] $ \rate -> urun (intfire `with` ["rate" =: dbl rate] ) (rate/10)
  urun (intfire) 0
  --print gsyn
  let peakgsyn = peak gsyn
      roi = fst <$$> inout (peakgsyn) (later 20e-3 $ peakgsyn)
  plotSig . head $ applyOverWith (/) gsyn roi
  --plotSig $ section gsynn (0, 20e-3, ())
  print . area $  (flip (/) <$$> roi) `applyOver` gsyn
  deleteCurrentSession

main = unsafeMain 

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
  io . print $ regress q 
  return ()


io = liftIO
-- make gain plot
-- post-spike signals like in fig 2

-- http://en.wikipedia.org/wiki/Regression_analysis
regress :: (Tagged t) => [t (Double,Double)] -> (Double,Double) --tag of type num,num
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


urun :: [Declare] -> Double -> IO ()
urun ds t0 = do
  Just sess <- readIORef unsafeSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess

--from samfun
mean :: Fractional a =>  [a] -> a
mean = go 0 0
        where
            -- go ::  -> Int -> [Double] -> Double
            go s n []     = s / fromIntegral n
            go !s !n (x:xs) = go (s+x) (n+1) xs
