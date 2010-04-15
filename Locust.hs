module Locust where

import StatsModel
import QueryUtils
--import LovSpikes hiding (main)
import NewSignal
import Control.Monad
import Math.Probably.Sampler
import TNUtils
import System.IO.Unsafe

type TPeak = Double
type Amp = Double
type NSpikes = Int
type LoV = Double
basePars = words "amp t0 tau1 tau2 tau3 pslow"

takeMany mp ks = for ks $ \k -> mp !!! k

parsWith s = map (++s) basePars

replicateData :: [(String, [Double])] -> Sampler (LoV, (TPeak,Amp,NSpikes))
replicateData sams = do 
  vs <- forM sams $ \(s,xs)-> fmap ((,) s) $ oneOf xs
  alpha <- mapM2 gauss (takeMany vs $ parsWith "") (takeMany vs $ parsWith "sd")
  beta <- mapM2 gauss (takeMany vs $ parsWith "beta") (takeMany vs $ parsWith "betasd")
  lov <-  uniform 0.01 0.05
  let lovl = (\lov-> log (lov / 0.02) / log 2) lov
  let centres = zipWith (\a b-> a+lovl*b) alpha beta
  pars <- mapM2 gauss centres (takeMany vs $ parsWith "trsd")
  let thisr = rFromPars pars 
  let rsig  = fillSig 0 6 0.001 thisr
  let [(tpeak, peakamp)]  = peak [rsig]
  nspikes <- length `fmap` sIPevSam rsig
  if tpeak >3  --FUDGE!!!!
     then nspikes `seq` tpeak `seq` peakamp `seq` return (lov, (tpeak,peakamp, nspikes))
     else replicateData sams

unsafeYrep :: [(String, [Double])] -> Int -> [(LoV, (TPeak,Amp,NSpikes))]
unsafeYrep mp n = unsafePerformIO $ fmap (take n) $ runSamplerIO $ replicateData mp

sel :: (b->c) -> [(a,b)] -> [(a,c)]
sel f = map $ onSnd f



--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

--integralR :: (Double, Double, Double, Double) -> Double -> Double
--integralR pars@(rate, tau, baseline, t0) t 
--    | t>t0 = integralR pars t0 + baseline * t
--    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

--http://www.wolframalpha.com/input/?i=a*(1-Exp[(x-t0)/t1])*((1-p)*Exp[(x-t0)/t2]%2Bp*Exp[(x-t0)/t3])
integralR :: [Double]-> Double -> Double
integralR pars@[amp, t0, tau1, tau2, tau3, pslow] t 
    | t>t0 = integralR pars t0 + 0.02 * (t-t0)
    | otherwise = let bigterm tau = tau * exp ((tau1*t - t0*(tau1+tau)) /(tau1*tau)) * 
                                    ((tau1+tau)* exp (t0/tau1) - tau1 * exp (t/tau1)) / (tau1+tau) 
		  in amp * (pslow * bigterm tau3 - (pslow-1) * bigterm tau2) 


r :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
r amp t0 tau1 tau2 tau3 pslow  t 
    | t < t0 = let x = (-t+t0) in 
               amp*(1-exp(-x/tau1))*((1-pslow)*exp(-x/tau2)+pslow*exp(-x/tau3))
    | otherwise = 0.02


logr :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
logr amp t0 tau1 tau2 tau3 pslow t 
    | t < t0 = let x = (-t+t0) in 
               log amp + log (1-exp(-x/tau1)) + log ((1-pslow)*exp(-x/tau2)+pslow*exp(-x/tau3))
    | otherwise = log 0.02


rFromPars :: [Double] -> Double -> Double
rFromPars [amp, t0, tau1, tau2, tau3, pslow] = r amp t0 tau1 tau2 tau3 pslow 
