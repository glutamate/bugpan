{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances, ForeignFunctionInterface #-}
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
import qualified Data.Array.Vector.UArr as UA
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

metSample x0 = metSample1 (mutGaussAbs x0 0.05)

--tst = fmap (take 100) $ runSamplerIO $ metSample (log . P.gaussD 0 1) 0.1

up_trial thedata p@(poppars, taus@(tau, baseline, t0), sessRates, trialRates) = do
  newtrialRates <- sampleMany2 $ forIdx2 trialRates $ \i j-> metSample 200 (\r-> 
                      likelihoodH1 (thedata!!i!!j) taus r + p_ij_i (sessRates!!i) poppars r) (trialRates!!i!!j)
  return (poppars, (tau, baseline, t0), sessRates, newtrialRates)

up_session (poppars, (tau, baseline, t0), sessRates, trialRates) = do
  newsessRates <- sampleMany $ forIdx sessRates $ \i -> 
                     metSample 200 (\sr-> 
                       (sum $ for (trialRates!!i) $ p_ij_i sr poppars) + 
                       p_i_pop poppars sr) (sessRates!!i)
  return (poppars, (tau, baseline, t0), newsessRates, trialRates)

up_pop thedata p@((poprate, popRateSD, trialRateSD), taus, sessRates, trialRates) = do
  (newpoprate, newpopsd) <- metSample (200,20) (\(pr, psd)-> (sum $ for sessRates $ p_i_pop (pr,psd, undefined)) + 
                                             p_pop pr psd) (poprate, popRateSD)
  newtaus <- metSample (0.005,0.002,0.1) (\ts-> sum $ map sum $ forIdx2 trialRates $ \i j->
                                       likelihoodH1 (thedata!!i!!j) ts (trialRates!!i!!j) --prior is uniform
                                   ) taus
  return ((newpoprate, newpopsd, trialRateSD), newtaus, sessRates, trialRates)

p_ij_i sr (_,_,sd) =  log . P.gaussD sr sd

p_i_pop (mu,sd,_) = log . P.gaussD mu sd

p_pop _ sd = lrsq sd

likelihoodH1 spikes (tau, baseline, t0) rate =
    let pars = (rate, tau, baseline, t0) in 
--    likelihood (realToFrac rate) (realToFrac tau) (realToFrac baseline) (realToFrac t0) 
    (sumU $ (mapU (log . r rate tau baseline t0) spikes))- integralR pars 6

gibbsSF thedata = condSampler (up_trial thedata) >>> condSampler up_session >>> condSampler (up_pop thedata )

priorSamplerG nsess ntrialsPerSess= 
    do poprate <- uniform 50 400
       popratesd <- uniform 2 40
       trRateSD <- return 20
       tau <- uniform 0.05 0.4
       baseline <- uniform 0 1
       t0 <- uniform 4.5 5.5
       sessrates <- times nsess $ gauss poprate popratesd
       trrates <- forM ntrialsPerSess $ \ntrs -> {-toU `fmap`-} (times ntrs $ gauss poprate trRateSD)
       return ((poprate,popratesd, trRateSD), (tau, baseline, t0), sessrates, trrates)

chopData2 :: (ChopByDur obs,Shiftable obs) => [Duration [Int]] -> obs -> [[obs]]
chopData2 durs allspikes = 
    let sessInts = nub $ map (head . snd) durs
    in for sessInts $ \i-> chopAndReset ((!!1) <$$> (((==i) .head)//durs)) allspikes

lrsq = log . recip . (\x-> x*x)

r :: Double -> Double -> Double -> Double -> Double -> Double
r rate tau baseline t0 t 
    | t < t0 = let t' = (t-t0)/tau 
               in (negate t')*exp(1+t')*(rate-baseline)+baseline
    | otherwise = baseline

--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

integralR :: (Double, Double, Double, Double) -> Double -> Double
integralR pars@(rate, tau, baseline, t0) t 
    | t>t0 = integralR pars t0 + baseline * t
    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

main :: IO ()
main = do
  (read -> count) : filenm : _  <- getArgs 
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
  let nthreads = numCapabilities
  putStrLn $ "splitting into nthreads="++show nthreads
  inits <- fmap (take nthreads) $ runSamplerIO $ priorSamplerG (length sess) (map (length . (`during` running) . (:[])) sess)
  let thespikes = chopData2 segs (toU spikes)
  writeFile (filenm++"_parnames.mcmc") $ show ["poprate", "popRateSD", "tau", "baseline", "t0"]
                            -- , "trialRateSD", "tau", "baseline", "t0"]
  inPar nthreads $ \threadn-> do
    let baymarkov = Mrkv (gibbsSF thespikes) (inits!!threadn) id
    let ofInterest ((poprate, popRateSD, trialRateSD), (tau, baseline, t0), _, _) = 
            [poprate, popRateSD, tau, baseline, t0]--, trialRateSD, tau, baseline, t0]  
    print $ ofInterest (inits!!threadn) 
    ps <- take count `fmap` runMarkovIO baymarkov

    writeInChunks (filenm++"_chain"++show threadn) 20000 $ map ofInterest ps
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

