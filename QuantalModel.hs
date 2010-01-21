{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances #-}
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

priorSamplerH nsess ntrialsPerSess = 
    do nmax <- round `fmap` uniform 5 30
       np <- uniform 0.01 0.99 
       qmean <- uniform 0 40
       qsd <- uniform 0 10
       pmean <- uniform 0.01 0.99
       psd <- uniform 0.1 0.4
       pupmean <- uniform 1 4
       pupsd <- uniform 0.1 0.7
       pdownmean <- uniform 1 4
       pdownsd <- uniform 0.1 0.7
       cv <- uniform 0 1
       sessns <- times nsess $ binomial nmax np
       sessps <- times nsess $ gauss pmean psd
       sessqs <- times nsess $ gauss qmean qsd
       sesspup <- times nsess $ gauss pupmean pupsd
       sesspdown <- times nsess $ gauss pdownmean pdownsd
       trnrels <- forM (zip3 ntrialsPerSess sessns sessps) $ \(ntrs, nsess, psess) -> toU `fmap` (times ntrs $ binomial nsess psess)
       return (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
               (pupmean, pupsd, pdownmean, pdownsd), 
               (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels)


hyperPriorPDF (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
               (pupmean, pupsd, pdownmean, pdownsd), 
               (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels) 
    =  sum $ map lrsq [qsd,  psd, qsd, pupsd, pdownsd]
    where lrsq = log . recip . (\x-> x*x)


sessPriorPDF (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
               (pupmean, pupsd, pdownmean, pdownsd), 
               (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels) =
    (sum $ map (log . P.gaussD qmean qsd) sessqs) +
    (sum $ map (log . P.gaussD pmean psd) sessps) +
    (sum $ map (log . P.gaussD pupmean pupsd) sesspup) +
    (sum $ map (log . P.gaussD pdownmean pdownsd) sesspdown) +
    (sum $ map (log . P.binomial nmax np) sessns) -- may not be correct to use prob mass function here 
    
trialPriorPDF (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
               (pupmean, pupsd, pdownmean, pdownsd), 
               (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels) =
    sum $ map (\(sn, sp, nrels) -> sumU $ mapU (log . P.binomial sn sp) nrels) $ zip3 sessns sessps trnrels

--http://mathworld.wolfram.com/NormalSumDistribution.html
likelihoodH st@[session, calevel, trial] ((t1t2,epsc):_) 
                (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
                 (pupmean, pupsd, pdownmean, pdownsd), 
                 (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels) 
    = let nrel = (trnrels!!session) `UA.indexU` trial 
          p = case calevel of
                0 -> sessps!!session
                1 -> (sessps!!session)*(sesspup !!session)
                2 -> (sessps!!session)/(sesspdown!!session)
          q = sessqs!!session
          var = (q*cv)*(q*cv)
      in  log $ P.gaussD (realToFrac nrel*q) (sqrt (realToFrac nrel*var)) epsc
likelihoodH st epdur theta = error $ "lhH with :"++show (st,epdur,theta)

proposalH =mutGauss 0.001 

main :: IO ()
main = do
  (read -> count) : _  <- getArgs 
  --let dropn = (count*3) `div` 4
  --putStrLn $ "droping "++show dropn
  (concat -> epscs, 
   concat -> running, 
   concat -> sess,
   concat -> calevels) <- fmap unzip4 $ manySessionData $ do
           epscs <- durations "epsc" (1::Double)
           running <- durations "running" ()
           modNm <- durations "moduleName" "foo"
           sess <- sessionDur
           -- unitDurationsStrict "running" >>= (io . print)
           let pnormalDur = oneDur $ take 20 running
           let phighDur = oneDur $ take 20 $ drop 20 running
           let plowDur = oneDur $ take 20 $ drop 40 running
           whenMaybe (not . null $ (=="simulateQuantalSyns")//modNm) $ 
                     return (epscs, running, sess, pnormalDur++phighDur++plowDur) 
--  (spikes, running, sess) <- inApproxSession "poisson0" $ do
                               
  --print $ length $ spikes
  --print $ (meanSDF `runStat` spikes)
  let segs = (distinct running) `within` ((distinct calevels) `within` (distinct sess))
  --print $ zip (take 10 segs) (take 10 epscs)
  let lh = manyLikeH segs likelihoodH epscs
  let nthreads = numCapabilities
  let foo = undefined `asTypeOf` hyperPriorPDF `asTypeOf` sessPriorPDF `asTypeOf` trialPriorPDF `asTypeOf` lh
  putStrLn $ "splitting into nthreads="++show nthreads
  inits <- fmap (take nthreads) $ runSamplerIO $ priorSamplerH (length sess) (map (length . (`during` running) . (:[])) sess)
  writeFile "quantal_parnames.mcmc" $ show ["nmax", "np", "qmean", "qsd", "pmean", "psd", "cv"]
  inPar nthreads $ \threadn-> do
    let bayfun = bayesMetLog (mutGauss 0.0003) [hyperPriorPDF, sessPriorPDF, trialPriorPDF, lh]
    let baymarkov = Mrkv bayfun (inits!!threadn) id
    ps <- take count `fmap` runMarkovIO baymarkov
    let ofInterest (((nmax,np), (qmean, qsd), (pmean, psd), cv), 
               (pupmean, pupsd, pdownmean, pdownsd), 
               (sessns, sessps, sessqs, (sesspup, sesspdown)), trnrels) = 
            [realToFrac nmax, np, qmean, qsd, pmean, psd, cv]  
    writeInChunks ("quantal_chain"++show threadn) 20000 $ map ofInterest ps
    --writeFile ("poisson_chain"++show threadn++"lastpar.mcmc") $ show $ last ps
    

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