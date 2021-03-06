{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fvia-c -optc-O3 #-}
{- INCLUDE "poisson.h" #-}
{- INCLUDE "poisson.c" #-}
{- CFILES poisson.c #-}

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
import qualified Data.Vector.Unboxed as U
import Data.Binary
import GHC.Conc
import StatsModel
import Foreign.Storable
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Arrow


sampleMany :: [Sampler a] -> Sampler [a]
sampleMany = sequence

sampleMany2 :: [[Sampler a]] -> Sampler [[a]]
sampleMany2 = sequence . map sequence

forIdx ::  [a] -> (Int ->  b) ->[b]
forIdx xss f = map (f . snd) $ zip xss [0..]

forIdx2 :: [[a]] -> (Int -> Int -> b) -> [[b]]
forIdx2 xss f = map g $ zip xss [0..]
    where g (xs, i) = map  (f i . snd) $ zip xs [0..]
 
for2 :: [[a]] -> (a->b) -> [[b]]
for2 xss f = map (map f) xss 


metSampleP = metSample1P depSam
metSamplePx0 x0  = metSample1P (depSamx0 x0)
metSamplePCL = metSample1PCL depSam

depSam w x0 =  mutGaussAbs x0 $ w*0.005
depSamx0 x0 w _ =  mutGaussAbs x0 $ w*0.005

--tst = fmap (take 100) $ runSamplerIO $ metSample (log . P.gaussD 0 1) 0.1

type AllTrialPars = [[Param TrialPar]]
type TrialPar = [Double]
type TrialSDs = [Param Double]
type SessMeans =  [Param TrialPar]
--type SessSDs = [TrialPar]
type PopMeansSds = [Param (Double,Double)]
--type PopSDs = Param TrialPar

type BigPar = ((PopMeansSds, TrialSDs), SessMeans, AllTrialPars) 

--[amp, t0, tau1, tau2, tau3, pslow, baseline]
fixPars = [800, 5, 0.3, 0.5,0.3,0.1]
fixPopSds = [40, 0.01, 0.05, 0.05, 0.03, 0.03]

allMul x = map (x*)

--up_trial thedata p@(poppars, taus@(tau, baseline, t0), sessRates, trialRates) = do
up_trial :: [[U.Vector Double]]-> BigPar -> Sampler BigPar
up_trial thedata ((popmeanssds, trialsds), sessmeans, trialPars) = do
  newtrialPars <- sampleMany2 $ forIdx2 trialPars $ \sess tr-> metSamplePCL
                      (likelihoodH1 (thedata!!sess!!tr))
                      (p_ij_i (unP $ sessmeans!!sess) (map unP trialsds)) (trialPars!!sess!!tr)
  return ((popmeanssds, trialsds), sessmeans, newtrialPars)

up_session :: BigPar -> Sampler BigPar
up_session ((popmeanssds, trialsds), sessmeans, trialPars) = do
  newsessmeans <- sampleMany $ forIdx sessmeans $ \sess -> 
                     metSampleP (\sessmean-> 
                       (sum $ for (map unP $ trialPars!!sess) $ p_ij_i (sessmean) (map unP trialsds)) +
                       p_i_pop (map ( fst . unP) popmeanssds) (map (snd . unP) popmeanssds) sessmean) (sessmeans!!sess)
  return ((popmeanssds, trialsds), newsessmeans, trialPars)

up_pop :: [[U.Vector Double]]-> BigPar -> Sampler BigPar
up_pop thedata ((popmeanssds, trialsds), sessmeans, trialPars) = do
  newpopmeanssds <- sampleMany $ forIdx (popmeanssds) $ \msi->
                        metSampleP (\(pm, ps)-> let pmean = set msi pm $ map (fst. unP) popmeanssds 
                                                    psd = set msi ps $ map (snd . unP) popmeanssds  in
                                                (sum $ for sessmeans $ p_i_pop pmean psd . unP ) + 
                                                ling ps ) $ popmeanssds!!msi
  newtrialsds <- sampleMany $ forIdx trialsds $ \si -> 
                        metSampleP (\sd-> (sum $ map sum $ forIdx2 trialPars $ \sess tr->
                                       let trsds = set si sd $ map unP trialsds in
                                       p_ij_i (unP $ sessmeans!!sess) trsds (unP $ trialPars!!sess!!tr)) +
                                       (ling sd)
                                   ) (trialsds!!si)
  return ((newpopmeanssds, newtrialsds), sessmeans, trialPars)

p_ij_i means sds pars =  sum $ map (\(mu, sd, x) ->  P.logGaussD mu sd x) $ zip3 means sds pars

p_i_pop = p_ij_i

p_pop _ sds = sum $ map ling sds

ling =  const 1 --log . invGammaD 0.001 0.001

set 0 x (_:ys) = x:ys
set n x (y:ys) = y : set (n-1) x ys

rFromPars :: TrialPar -> Double -> Double
rFromPars [amp, t0, tau1, tau2, tau3, pslow] = r amp t0 tau1 tau2 tau3 pslow 0.2

likelihoodH1 spikes pars@[amp, t0, tau1, tau2, tau3, pslow] =
--    likelihood (realToFrac rate) (realToFrac tau) (realToFrac baseline) (realToFrac t0) 
    (U.sum $ (U.map (log . r amp t0 tau1 tau2 tau3 pslow 0.2) spikes))- (integralR pars 6 - integralR pars 0)

gibbsSF thedata = condSampler (up_trial thedata) >>> condSampler up_session >>> condSampler (up_pop thedata )

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = mapM (uncurry f) $ zip xs ys

--[amp, t0, tau1, tau2, tau3, pslow, baseline]
priorSamplerG :: Int -> [Int] -> Sampler BigPar
priorSamplerG nsess ntrialsPerSess= 
    let k = 1.3 in 
    do popmeans <- mapM2  uniform  (map (/k) fixPars) (map (*k) fixPars)
       popsds <- mapM2 uniform (map (/k) fixPopSds) (map (*k) fixPopSds)
       trialsds <- mapM2 uniform (map (/k) fixPopSds) (map (*k) fixPopSds)
       sessmeans <- times nsess $ mapM2 gauss popmeans popsds
       trialpars <- forM ntrialsPerSess $ \ntrs -> (times ntrs $ mapM2 gauss popmeans popsds)
       return ((map newParam $ zip popmeans popsds, map newParam trialsds), map newParam sessmeans, map (map newParam) trialpars)

chopData2 :: (ChopByDur obs,Shiftable obs) => [Duration [Int]] -> obs -> [[obs]]
chopData2 durs allspikes = 
    let sessInts = nub $ map (head . snd) durs
    in for sessInts $ \i-> chopAndReset ((!!1) <$$> (((==i) .head)//durs)) allspikes

lrsq = log . recip . (\x-> x*x)

r :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
r amp t0 tau1 tau2 tau3 pslow baseline t 
    | t < t0 = let x = (-t+t0) in 
               amp*(1-exp(-x/tau1))*((1-pslow)*exp(-x/tau2)+pslow*exp(-x/tau3)) + baseline
    | otherwise = baseline

--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

--integralR :: (Double, Double, Double, Double) -> Double -> Double
--integralR pars@(rate, tau, baseline, t0) t 
--    | t>t0 = integralR pars t0 + baseline * t
--    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

--http://www.wolframalpha.com/input/?i=a*(1-Exp[(x-t0)/t1])*((1-p)*Exp[(x-t0)/t2]%2Bp*Exp[(x-t0)/t3])
integralR :: [Double]-> Double -> Double
integralR pars@[amp, t0, tau1, tau2, tau3, pslow] t 
    | t>t0 = integralR pars t0 + 0.2* (t-t0)
    | otherwise = let bigterm tau = tau * exp ((tau1*t - t0*(tau1+tau)) /(tau1*tau)) * 
                                    ((tau1+tau)* exp (t0/tau1) - tau1 * exp (t/tau1)) / (tau1+tau) 
		  in amp * (pslow * bigterm tau3 - (pslow-1) * bigterm tau2) + 0.2 * t


ofInterest :: BigPar -> [Double]
ofInterest ((popmeanssds, trialsds), sessmeans, trialPars) = 
    (map (fst . unP) popmeanssds) ++ (map (snd . unP) popmeanssds) ++ (map unP trialsds) ++ (unP $ head sessmeans) ++( unP $ head $ head trialPars)

-- ++popsds -- ++trialsds

parNames = words $ "amp t0 tau1 tau2 tau3 pslow "++
                   "amppopsd t0popsd tau1popsd tau2popsd tau3popsd pslowpopsd "++
                   "amptrsd t0trsd tau1trsd tau2trsd tau3trsd pslowtrsd "++
                   "amps1mean t0s1mean tau1s1mean tau2s1mean tau3s1mean pslows1mean "++ 
                   "amps1tr1 t0s1tr1 tau1s1tr1 tau2s1tr1 tau3s1tr1 pslows1tr1"

all2 :: (a->Bool) -> [[a]] -> Bool
all2 p = and . map (all p)

zip2d :: [[a]] -> [[b]] -> [(a,b)]
zip2d xss yss = concat $ zipWith zip xss yss

last3 [x,y,z] = [x,y,z]
last3 [] = []
last3 (x:xs) = last3 xs

main :: IO ()
main = do
  (read -> count::Int) : filenm : _  <- getArgs 
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
  --let arr = SV.pack [1,2,3]
  --let (fp, n1, n2) = SVB.toForeignPtr arr
  --SVB.withStartPtr arr $ \p n -> do
  --  print $ test_sum p $ fromIntegral n

  let segs = (distinct running) `within` (distinct sess)
  --let lh = undefined -- manyLikeH segs likelihoodH id (toU spikes)
  let thespikes = chopData2 segs (U.fromList spikes)
  let nthreads = numCapabilities
  putStrLn $ "splitting into nthreads="++show nthreads
  let acceptInit pars = all notNanInf $ map (uncurry likelihoodH1) $ zip2d thespikes $ map ( map unP ) $ trd3 pars
  inits <- fmap (filter acceptInit) $ runSamplerIO $ priorSamplerG (length sess) (map (length . (`during` running) . (:[])) sess)
  writeFile (filenm++"_parnames.mcmc") $ show parNames 
                            -- , "trialRateSD", "tau", "baseline", "t0"]
  inPar nthreads $ \threadn-> do
    let baymarkov = Mrkv (gibbsSF thespikes) (inits!!threadn) id
    print2 "initials: " $ ofInterest (inits!!threadn) 
    --print2 "spikes: " $ thespikes !!0 !!0
    --print2 "pars: " (head $ head $ trd3 $ inits!!threadn)
    --print2 "r(4) = " $ rFromPars (head $ head $ trd3 $ inits!!threadn) 1
    --print2 "L: " $ likelihoodH1 (thespikes!!0!!0) (head $ head $ trd3 $ inits!!threadn)

    ps <- take count `fmap` runMarkovIO baymarkov
    --gnuplotOnScreen $ (FunSeg 0 6 $ integralR fixPars) -- (head $ head $ trd3 $ inits!!threadn)
    --print $ euler 0.0001 0 6 $ rFromPars fixPars
    --print $ integralR fixPars 6
    writeInChunks (filenm++"_chain"++show threadn) 20000 $ map ofInterest ps
    --mapM print (map (head . head . trd3) $ last3 ps)
    
    --writeFile ("poisson_chain"++show threadn++"lastpar.mcmc") $ show $ last ps
    --mapM print $ map ofInterest ps
    return () 

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



euler :: Double -> Double -> Double -> (Double -> Double) -> Double
euler h t1 t2 f = let ts = [t1, t1+h..t2]
                      g acc t =  acc+h*f(t) 
                  in foldl g 0 ts