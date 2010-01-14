{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}

module Main where

import HaskSyntaxUntyped
import Expr
import EvalM
import Numbers
import System.Environment
import Data.Char
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


priorSampler = 
    do rate <- uniform 0 300
       tau <- uniform 0.01 0.3
       baseline <- uniform 0 1
       t0 <- uniform 4.5 5.5
       return (rate, tau, baseline, t0)

priorSamplerH nsess ntrialsPerSess= 
    do poprate <- uniform 0 300
       popratesd <- uniform 0 60
       trRateSD <- uniform 0 60
       tau <- uniform 0.01 0.3
       baseline <- uniform 0 1
       t0 <- uniform 4.5 5.5
       sessrates <- times nsess $ gauss poprate popratesd
       trrates <- forM (zip ntrialsPerSess sessrates) $ \(ntrs, sr) -> times ntrs $ gauss sr trRateSD
       return ((poprate,popratesd, trRateSD), (tau, baseline, t0), sessrates, trrates)


priorPDF (rate, tau, baseline, t0) | between 0 300 rate && between 0.01 0.3 tau &&
                                       between 0 1 baseline && between 4.5 5.5 t0 =  0
                                   | otherwise = - 1e20
    where between l u x = x > l && x < u

hyperPriorPDF ((poprate, popRateSD, trialRateSD), (tau, baseline, t0), _, _) 
    | between 0 300 poprate && between 0.01 0.3 tau &&
      between 0 1 baseline && between 4.5 5.5 t0 && 
      between 0 60 trialRateSD && between 0 60 popRateSD =  0
    | otherwise = - 1e20
    

sessPriorPDF ((poprate, popratesd, _), _, sessRates, _) =
    sum $ map (\sr -> log $ P.gauss poprate popratesd sr) sessRates

trialPriorPDF ((_, _, trialRateSD), _, sessRates, trialRates) =
    sum $ map (\(sr, trRates) -> sum $ map (log . P.gauss sr trialRateSD) trRates) $ zip sessRates trialRates
    

likelihoodH [session, trial] spikes (_, (tau, baseline, t0), sessRates, trialRates) =
    let pars = (trialRates!!session!!trial, tau, baseline, t0) in
    ((sum $ (map (log . r pars . fst) spikes))- integralR pars 6  )

likelihood pars@(rate, tau, baseline, t0) spikes =
    ((sum $ (map (log . r pars . fst) spikes))- integralR pars 6  )

manyLikeH :: (ChopByDur obs,Shiftable obs) => 
             [Duration [Int]] -> 
            ([Int] -> obs -> P.PDF theta) -> 
            (obs -> P.PDF theta)
manyLikeH durs lh1 = \obs-> \theta-> sum $ map (\(obs, (_,ints)) -> lh1 ints obs theta) $ zip (chopAndReset durs obs) durs

within :: [Duration [Int]] -> [Duration [Int]] -> [Duration [Int]]
within short long = concatMap f $ relabelWithin long short 
    where f d@(t1t2, sints) = case sectionDur1 d long of 
                                  []-> []
                                  (_,lints):_ -> [(t1t2, lints++sints)]

distinct :: [Duration a] -> [Duration [Int]]
distinct durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]


relabelWithin :: [Duration a] -> [Duration [Int]] -> [Duration [Int]]
relabelWithin long short = concatMap (f . (:[])) long
    where f onelong = (:[]) <$$> (tagMany [0..] $ during onelong short)

--distinctWithin :: [Duration a] -> [Duration [Int]]
--distinctWithin durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]


r (rate, tau, baseline, t0) t 
    | t < t0 = ((-(t-t0)/tau)*exp(1+(t-t0)/tau))*(rate-baseline)+baseline
    | otherwise = baseline

--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

integralR pars@(rate, tau, baseline, t0) t 
    | t>t0 = integralR pars t0 + baseline * t
    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

manyLikeOver :: (ChopByDur obs,Shiftable obs) => [Duration a] -> (theta -> P.PDF obs) -> (obs -> P.PDF theta)
manyLikeOver durs lh1 = \obs-> \theta-> sum $ map (lh1 theta) $ chopAndReset durs obs 


proposal (rate, tau, baseline, t0) =
    do nrate <- gauss rate 0.1
       ntau <- gauss tau 0.0005
       nbaseline <- gauss baseline 0.001
       nt0 <- gauss t0 0.005
       return (nrate, ntau, nbaseline, nt0)


proposalH = --(poprate, popratesd, trialRateSD, tau, baseline, t0, sessRates, trialRates) =
    mutGauss 0.001 
    {-do npoprate <- gauss rate 0.1
       ntau <- gauss tau 0.0005
       nbaseline <- gauss baseline 0.001
       nt0 <- gauss t0 0.005
       return (nrate, ntau, nbaseline, nt0) -}

class MutateGaussian a where
    mutGauss :: Double -> a -> Sampler a

instance MutateGaussian Double where
    mutGauss cv x = gauss x (cv*x)

instance MutateGaussian a => MutateGaussian [a] where
    mutGauss cv xs = mapM (mutGauss cv) xs

instance (MutateGaussian a, MutateGaussian b) => MutateGaussian (a,b) where
    mutGauss cv (x,y) = liftM2 (,) (mutGauss cv x) (mutGauss cv y)

instance (MutateGaussian a, MutateGaussian b, MutateGaussian c) => MutateGaussian (a,b,c) where
    mutGauss cv (x,y,z) = liftM3 (,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z)

instance (MutateGaussian a, MutateGaussian b, MutateGaussian c, MutateGaussian d) => MutateGaussian (a,b,c,d) where
    mutGauss cv (x,y,z,w) = liftM4 (,,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z) (mutGauss cv w)


eq = nearly 1e-8

eqpar (rate, tau, baseline, t0) (rate1, tau1, baseline1, t01) =
    eq rate rate1 && eq tau tau1 && eq baseline baseline1 && eq t0 t01 

main :: IO ()
main = do
  (concat -> spikes, concat -> running, concat -> sess) <- fmap unzip3 $ manySessionData $ do
           spikes <- events "spike" ()
           running <- durations "running" ()
           modNm <- durations "moduleName" "foo"
           sess <- sessionDur
           whenMaybe (not . null $ (=="simulatePoissonSpikes")//modNm) $ 
                     return (spikes, running, sess) 
--  (spikes, running, sess) <- inApproxSession "poisson0" $ do
                               
  let segs = (distinct running) `within` (distinct sess)
  let lh = manyLikeH segs likelihoodH spikes
  --let bayfin = bayesMetLog 
  --mapM print sess
  inits <- fmap head $ runSamplerIO $ priorSamplerH (length sess) (map (length . (`during` running) . (:[])) sess)
  putStrLn $ "inits "++show (lh inits, fst4 inits)
  --putStrLn "segs"
  --print segs
  let bayfun = bayesMetLog (mutGauss 0.0005) [hyperPriorPDF, sessPriorPDF, trialPriorPDF, lh]
  let baymarkov = Mrkv bayfun inits id
  ps <- (take 400) `fmap` runMarkovIO baymarkov
  let noburn = {-drop 15000 -} ps

  let plotWith nm f =  (nm, Lines $ zip [(0::Double)..] $ map f ps)
              
  putStrLn $ "poprate "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (fst3 . fst4)  noburn) 
  putStrLn $ "popratesd "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (snd3 . fst4)  noburn) 
  putStrLn $ "trialRateSD "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (trd3 . fst4)  noburn) 
  putStrLn $ "tau "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (fst3 . snd4) noburn)
  putStrLn $ "baseline "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (snd3 . snd4) noburn)
  putStrLn $ "t0 "++ show ((meanSDF `both` nSumSumSqr) `runStat` map (trd3 . snd4) noburn)

  --gnuplotOnScreen $ (plotWith "rate" (fst3 . fst4)  :||: plotWith "tau" (fst3 . snd4)) :==: 
   --                   (plotWith "baseline" (snd3 . snd4) :||: plotWith "t0" (trd3 . snd4)) 

  --mapM print $ map (\p-> (lh p, fst4 p, snd4 p)) $ lastn 20 noburn 
  return ()

main1 :: IO ()
main1 = 
    do inSessionFromArgs $ do          
         spike <- events "spike" ()
         running <- durations "running" ()
         ps <- io $ do
           let lh = (manyLikeOver running likelihood spike)
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
           gnuplotOnScreen $ (plotWith "rate" fst4  :||: plotWith "tau" snd4) :==: 
                               (plotWith "baseline" trd4 :||: plotWith "t0" fth4) 

           let meanLen = meanF `runStat` (map (realToFrac . length) $ groupBy eqpar noburn )
           putStrLn $ "mean length unchanging "++show meanLen
           --mapM print $ map (\p-> (lh p, priorPDF p, p)) $ lastn 100 noburn 
           --tst <- take 10 `fmap` runMarkovIO testMkv
           --mapM print tst
           --return ps
         
         
         return ()

lastn n xs = let len = length xs 
             in if n > len
                   then xs
                   else drop (len - n) xs


--between l u x = x > l && x < u


--mapIdx :: (Int -> b) -> [a] -> [b]
--mapIdx f xs = map f [0..length xs-1]
