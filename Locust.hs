{-# LANGUAGE ViewPatterns #-} 

module Locust where

import StatsModel
import QueryUtils
--import LovSpikes hiding (main)
import NewSignal
import Control.Monad
import Math.Probably.Sampler
import TNUtils
import System.IO.Unsafe
import Math.Probably.FoldingStats
import Math.Probably.Distribution
import ReactiveDistributions

type TPeak = Double
type Amp = Double
type NSpikes = Int
type LoV = Double
basePars = words "amp t0 tau1 tau2 tau3 pslow"

takeMany mp ks = for ks $ \k -> mp !!! k

parsWith s = map (++s) basePars

replicateAnimal :: [(String, [Double])] -> Sampler ([Double], [Double], [Double])
replicateAnimal sams = do 
  vs <- forM sams $ \(s,xs)-> fmap ((,) s) $ oneOf xs
  alpha <- mapM2 gauss (takeMany vs $ parsWith "") (takeMany vs $ parsWith "sd")
  beta <- mapM2 gauss (takeMany vs $ parsWith "beta") (takeMany vs $ parsWith "betasd")
  return (alpha, beta, (takeMany vs $ parsWith "trsd"))

animalMeasures :: LoV -> ([Double], [Double], [Double]) ->  Sampler (TPeak,Amp,NSpikes)
animalMeasures lov (alpha, beta, trsds) = do
  let centres = zipWith (\a b->  a+lovl lov*b) alpha beta
  pars <- mapM2 gauss centres trsds
  let rsig = parsToR pars
  let [(tpeak, peakamp)]  = peak [rsig]
  nspikes <- length `fmap` sIPevSam rsig
  if tpeak >3  --FUDGE!!!! 
     then nspikes `seq` tpeak `seq` peakamp `seq` return (tpeak,peakamp, nspikes)
     else animalMeasures lov (alpha, beta, trsds)

withinVariability1 :: Int -> LoV -> ([Double], [Double], [Double]) ->  Sampler (TPeak,Amp,Double)
withinVariability1 ntrs lov animal = do
  measures <- mapM (const $ animalMeasures lov animal) [1..ntrs]
  let sdTpeak = stdDevF `runStat` map fst3 measures
  let sdAmp = stdDevF `runStat` map snd3 measures
  let sdNspks = stdDevF `runStat` map (realToFrac . trd3) measures
  return (sdTpeak, sdAmp, sdNspks)
            
withinAndAcrossVariability :: Double -> Int -> Int ->  [(String, [Double])] -> Sampler [Double]
withinAndAcrossVariability lov nanimal ntrs sams = do
  animals <- mapM (const $ replicateAnimal sams) [1..nanimal]
  measuress <- forM animals $ \animal -> 
                 mapM (const $ animalMeasures lov animal) [1..ntrs]
  meansds <- forM measuress $ \measures-> do
                    let (mpeak, sdTpeak) = meanSDF `runStat` map ((5-) . fst3) measures
                    let (mamp, sdamp) = meanSDF `runStat` map snd3 measures
                    let (mnsp, sdnsp) = meanSDF `runStat` map (realToFrac . trd3) measures
                    return ([mpeak, mamp, mnsp],[sdTpeak, sdamp, sdnsp])
  let between = (fmap (\(m,sd) -> sd/m) meanSDF) `runStatOnMany` map fst meansds
  let avgs = meanF `runStatOnMany` map fst meansds
  let within = meanF `runStatOnMany` map snd meansds

  return $ between ++ (zipWith (/) within avgs)


lovl lov = log (lov / 0.02) / log 2

replicatePars :: [(String, [Double])] -> Sampler (LoV, [Double])
replicatePars sams = do 
  (alpha, beta, trsds) <- replicateAnimal sams
  lov <-  uniform 0.01 0.05
  let centres = zipWith (\a b->  a+lovl lov *b) alpha beta
  pars <- mapM2 gauss centres trsds
  return (lov,pars)

parsToR = fillSig 0 6 0.001 . rFromPars

replicateRsigs :: [(String, [Double])] -> Sampler (LoV, Signal Double)
replicateRsigs sams = do
  (lov, pars) <- replicatePars sams
  let thisr = rFromPars pars 
  let rsig  = fillSig 0 6 0.001 thisr
  return (lov, rsig)

replicateData :: [(String, [Double])] -> Sampler (LoV, (TPeak,Amp,NSpikes))
replicateData sams = do 
  (lov, rsig) <- replicateRsigs sams
  let [(tpeak, peakamp)]  = peak [rsig]
  nspikes <- length `fmap` sIPevSam rsig
  if tpeak >3  --FUDGE!!!! 
     then nspikes `seq` tpeak `seq` peakamp `seq` return (lov, (tpeak,peakamp, nspikes))
     else replicateData sams

unsafeYrep :: [(String, [Double])] -> Int -> [(LoV, (TPeak,Amp,NSpikes))]
unsafeYrep mp n = unsafePerformIO $ fmap (take n) $ runSamplerIO $ replicateData mp

unsafeRepPars mp n =  unsafePerformIO $ fmap (take n) $ runSamplerIO $ replicatePars mp

sel :: (b->c) -> [(a,b)] -> [(a,c)]
sel f = map $ onSnd f



--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

--integralR :: (Double, Double, Double, Double) -> Double -> Double
--integralR pars@(rate, tau, baseline, t0) t 
--    | t>t0 = integralR pars t0 + baseline * t
--    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

--http://www.wolframalpha.com/input/?i=a*(1-Exp[(x-t0)/t1])*((1-p)*Exp[(x-t0)/t2]%2Bp*Exp[(x-t0)/t3])
integralR :: [Double]-> Double -> Double
integralR pars@[amp, t0, tau1, tau2, tau3, pslowpar ] t 
    | t>t0 = integralR pars t0 + 0.02 * (t-t0)
    | otherwise = let pslow = sigmoid pslowpar 
                      bigterm tau = tau * exp ((tau1*t - t0*(tau1+tau)) /(tau1*tau)) * 
                                    ((tau1+tau)* exp (t0/tau1) - tau1 * exp (t/tau1)) / (tau1+tau) 
		  in amp * (pslow * bigterm tau3 - (pslow-1) * bigterm tau2) 


sigmoid x = 1/(1+exp (-x))

r :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
r amp t0 tau1 tau2 tau3 pslowpar  t 
    | t < t0 = let x = (-t+t0) 
                   pslow = sigmoid pslowpar
               in amp*(1-exp(-x/tau1))*((1-pslow)*exp(-x/tau2)+pslow*exp(-x/tau3))
    | otherwise = 0.02

alpha tau t = if t<0.0 then 0.0 else (t/tau) * exp(1 - t/tau)

tsalpha t0 tau t = alpha tau $ negate $ t - t0

{-logr :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
logr amp t0 tau1 tau2 tau3 pslow t 
    | t < t0 = let x = (-t+t0) in 
               log amp + log (1-exp(-x/tau1)) + log ((1-pslow)*exp(-x/tau2)+pslow*exp(-x/tau3))
    | otherwise = log 0.02 -}


rFromPars :: [Double] -> Double -> Double
rFromPars [amp, t0, tau1, tau2, tau3, pslow] = r amp t0 tau1 tau2 tau3 pslow 


janSampler tau2mean = do 
  t0' <- uniform 0 0.02
  let noise = 0.05
  tau1 <- gaussD 0.06 0.002
  amp1 <- gaussD 25 0.5
  tau2 <- gaussD 0.2 0.02
  amp2 <- gaussD 5 0.5
  offset <- gaussD 95 0.3
  let waveform t = offset - amp1 * alpha tau1 (t-(0.1+t0')) - amp2 * alpha tau2 (t-(0.1+t0'))
  let sig = fillSig 0 2 0.02 waveform
  sampler $ RandomSignal sig noise
