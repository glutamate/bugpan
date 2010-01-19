{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances #-}
{-# OPTIONS_GHC -fvia-c -optc-O3 #-}
module Main where

--import HaskSyntaxUntyped
--import Expr
import EvalM
--import Numbers
import System.Environment
--import Data.Char
import Data.List
import TNUtils
import Query 
import Control.Monad
import Math.Probably.Sampler
import Math.Probably.StochFun
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import QueryTypes
import Math.Probably.FoldingStats
import PlotGnuplot
import QueryPlots
import QueryUtils hiding (groupBy)
import Database
import Data.Array.Vector 
import qualified Data.Array.Vector.UArr as UA
import Data.Binary
import GHC.Conc
import StatsModel

priorSamplerH nsess ntrialsPerSess= 
    do poprate <- uniform 199 201
       popratesd <- uniform 19 21
       trRateSD <- uniform 29 31
       tau <- uniform 0.19 0.21
       baseline <- uniform 0.09 0.11
       t0 <- uniform 4.95 5.05
       sessrates <- times nsess $ gauss poprate popratesd
       trrates <- forM (zip ntrialsPerSess sessrates) $ \(ntrs, sr) -> toU `fmap` (times ntrs $ gauss sr trRateSD)
       return ((poprate,popratesd, trRateSD), (tau, baseline, t0), sessrates, trrates)


hyperPriorPDF ((poprate, popRateSD, trialRateSD), (tau, baseline, t0), _, _) 
    | between 0 300 poprate && between 0.01 0.3 tau &&
      between 0 1 baseline && between 4.5 5.5 t0 && 
      between 0 60 trialRateSD && between 0 60 popRateSD =  lrsq popRateSD + lrsq trialRateSD
    | otherwise = - 1e20
    where lrsq = log . recip . (\x-> x*x)


sessPriorPDF ((poprate, popratesd, _), _, sessRates, _) =
    sum $ map (log . P.gaussD poprate popratesd) sessRates

trialPriorPDF ((_, _, trialRateSD), _, sessRates, trialRates) =
    sum $ map (\(sr, trRates) -> sumU $ mapU (log . P.gaussD sr trialRateSD) trRates) $ zip sessRates trialRates
    
{-calcPars [session, trial] (_, (tau, baseline, t0), sessRates, trialRates) =
    (trialRates!!session!!trial, (tau, baseline, t0)-}

likelihoodH st@[session, trial] spikes bigp@(_, (tau, baseline, t0), sessRates, trialRates) =
    let pars = ((trialRates!!session) `UA.indexU` trial, tau, baseline, t0) in 
    (sumU $ (mapU (log . r pars) spikes))- integralR pars 6


r :: (Double, Double, Double,Double) -> Double -> Double
r (rate, tau, baseline, t0) t 
    | t < t0 = ((-(t-t0)/tau)*exp(1+(t-t0)/tau))*(rate-baseline)+baseline
    | otherwise = baseline

--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

integralR :: (Double, Double, Double, Double) -> Double -> Double
integralR pars@(rate, tau, baseline, t0) t 
    | t>t0 = integralR pars t0 + baseline * t
    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t



proposalH =mutGauss 0.001 




main :: IO ()
main = do
  (read -> count) : _  <- getArgs 
  --let dropn = (count*3) `div` 4
  --putStrLn $ "droping "++show dropn
  (concat -> spikes, concat -> running, concat -> sess) <- fmap unzip3 $ manySessionData $ do
           spikes <-  map fst `fmap` events "spike" ()
           running <- durations "running" ()
           modNm <- durations "moduleName" "foo"
           sess <- sessionDur
           whenMaybe (not . null $ (=="simulatePoissonSpikes")//modNm) $ 
                     return (spikes, running, sess) 
--  (spikes, running, sess) <- inApproxSession "poisson0" $ do
                               
  --print $ length $ spikes
  --print $ (meanSDF `runStat` spikes)
  let segs = (distinct running) `within` (distinct sess)
  let lh = manyLikeH segs likelihoodH $ toU spikes
  let nthreads = numCapabilities
  putStrLn $ "splitting into nthreads="++show nthreads
  inits <- fmap (take nthreads) $ runSamplerIO $ priorSamplerH (length sess) (map (length . (`during` running) . (:[])) sess)
  writeFile "poisson_parnames.mcmc" $ show ["poprate", "popRateSD", "trialRateSD", "tau", "baseline", "t0"]
  inPar nthreads $ \threadn-> do
    let bayfun = bayesMetLog (mutGauss 0.0003) [hyperPriorPDF, sessPriorPDF, trialPriorPDF, lh]
    let baymarkov = Mrkv bayfun (inits!!threadn) id
    ps <- take count `fmap` runMarkovIO baymarkov
    let ofInterest ((poprate, popRateSD, trialRateSD), (tau, baseline, t0), _, _) = 
            [poprate, popRateSD, trialRateSD, tau, baseline, t0] 
    writeInChunks ("poisson_chain"++show threadn) 20000 $ map ofInterest ps


  {-let noburn = drop dropn ps
 
  let plotWith nm f =  (nm, Lines $ zip [(0::Double)..] $ map f ps)
              
  putStrLn $ "poprate "++ show (meanSDF `runStat` map (fst3 . fst4)  noburn) 
  putStrLn $ "popratesd "++ show (meanSDF  `runStat` map (snd3 . fst4)  noburn) 
  putStrLn $ "trialRateSD "++ show (meanSDF `runStat` map (trd3 . fst4)  noburn) 
  putStrLn $ "tau "++ show (meanSDF `runStat` map (fst3 . snd4) noburn)
  putStrLn $ "baseline "++ show (meanSDF `runStat` map (snd3 . snd4) noburn)
  putStrLn $ "t0 "++ show (meanSDF `runStat` map (trd3 . snd4) noburn)

  gnuplotOnScreen $ (plotWith "poprate" (fst3 . fst4)  :||: plotWith "popratesd" (snd3 . fst4)) :==: 
                     (plotWith "baseline" (snd3 . snd4) :||: plotWith "trialratesd" (trd3 . fst4)) 
-}
  --mapM print $ map (\p-> (lh p, fst4 p, snd4 p)) $ lastn 20 noburn 
  --return ()


{-main1 :: IO ()
main1 = 
    do inSessionFromArgs $ do          
         spike <- map fst `fmap` events "spike" ()
         running <- durations "running" ()
         ps <- io $ do
           let lh = manyLikeOver running likelihood spike
           let bayfun = bayesMetLog proposal [manyLikeOver running likelihood spike, priorPDF]
           inits <- fmap head $ runSamplerIO priorSampler
           let baymarkov = Mrkv bayfun inits id
           ps <- (take 8000) `fmap` runMarkovIO baymarkov
         --bsam <- io $ bayes 100000 (manyLikeOver running likelihood spike) priorSampler
         --ps <- take 10000 `fmap` (io $ runSamplerIO bsam)
           putStrLn $ "inits "++show (lh inits, priorPDF inits, inits)
           let noburn = drop 2000 ps
           
           putStrLn $ "rate "++ show ((meanSDF `both` nSumSumSqr) `runStat` map fst4 noburn) 
           putStrLn $ "tau "++ show ((meanSDF `both` nSumSumSqr) `runStat` map snd4 noburn)
           putStrLn $ "baseline "++ show ((meanSDF `both` nSumSumSqr) `runStat` map trd4 noburn)
           putStrLn $ "t0 "++ show ((meanSDF `both` nSumSumSqr) `runStat` map fth4 noburn)
           
           --print $ (manyLikeOver running likelihood spike) inits

           let plotWith nm f =  (nm, Lines $ zip [(0::Double)..] $ map f ps)
           --putStrLn $ "true lh: "++show (lh (200, 0.2, 0.1, 5))
           --putStrLn $ "off lh: "++show (lh (201, 0.22, 0.09, 5.3))
           --gnuplotOnScreen $ (plotWith "rate" fst4  :||: plotWith "tau" snd4) :==: 
           --                    (plotWith "baseline" trd4 :||: plotWith "t0" fth4) 

           let meanLen = meanF `runStat` (map (realToFrac . length) $ groupBy eqpar noburn )
           putStrLn $ "mean length unchanging "++show meanLen
           --mapM print $ map (\p-> (lh p, priorPDF p, p)) $ lastn 100 noburn 
           --tst <- take 10 `fmap` runMarkovIO testMkv
           --mapM print tst
           --return ps
         
         
         return ()
likelihood pars@(rate, tau, baseline, t0) spikes =
    ((sum $ (map (log . r pars) spikes))- integralR pars 6  )



priorSampler = 
    do rate <- uniform 0 300
       tau <- uniform 0.19 0.21
       baseline <- uniform 0 1
       t0 <- uniform 4.5 5.5
       return (rate, tau, baseline, t0)


priorPDF (rate, tau, baseline, t0) | between 0 300 rate && between 0.01 0.3 tau &&
                                       between 0 1 baseline && between 4.5 5.5 t0 =  0
                                   | otherwise = - 1e20
    where between l u x = x > l && x < u

proposal (rate, tau, baseline, t0) =
    do nrate <- gauss rate 0.1
       ntau <- gauss tau 0.0005
       nbaseline <- gauss baseline 0.001
       nt0 <- gauss t0 0.005
       return (nrate, ntau, nbaseline, nt0)


eq = nearly 1e-8

eqpar (rate, tau, baseline, t0) (rate1, tau1, baseline1, t01) =
    eq rate rate1 && eq tau tau1 && eq baseline baseline1 && eq t0 t01 


--between l u x = x > l && x < u


--mapIdx :: (Int -> b) -> [a] -> [b]
--mapIdx f xs = map f [0..length xs-1]
-}