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
import StatsModel
import Foreign.Storable
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Arrow
import FitGnuplot
import Debug.Trace
import Locust



--tst = fmap (take 100) $ runSamplerIO $ metSample (log . P.gaussD 0 1) 0.1

type AllTrialPars = [[Param TrialPar]]
type TrialPar = [Double]
type TrialSDs = [Param Double]
type SessMeans =  [Param TrialPar]
type SessBetas =  [Param TrialPar]
--type SessSDs = [TrialPar]
type PopMeansSds = [Param (Double,Double)]
type BetaMeansSds = [Param (Double,Double)]
--type PopSDs = Param TrialPar

type BigPar = ((PopMeansSds, TrialSDs, BetaMeansSds), SessMeans, SessBetas, AllTrialPars) 

type TheData = [[(U.Vector Double, Double)]]

--[amp, t0, tau1, tau2, tau3, pslow, baseline]
fixPars = [210, 4.9, 6.83e-2, 0.27,1,0.15]
fixPopSds = [10, 0.01, 6.83e-3, 0.02, 0.1, 0.02]

allMul x = map (x*)

--up_trial thedata p@(poppars, taus@(tau, baseline, t0), sessRates, trialRates) = do
up_trial :: TheData -> BigPar -> Sampler BigPar
up_trial thedata ((popmeanssds, trialsds, betas), sessmeans, sessbetas, trialPars) = do
  newtrialPars <- sampleMany2 $ forIdx2 trialPars $ \sess tr-> 
                  let (spikes, lov) = (thedata!!sess!!tr) in metSamplePCL "lh"
                      (likelihoodH1 spikes)
                      (p_ij_i (zipWith (+) (unP $ sessmeans!!sess) (map (lov*) (unP $ sessbetas!!sess))) 
                              (map unP trialsds)) (trialPars!!sess!!tr)
  return ((popmeanssds, trialsds, betas), sessmeans, sessbetas, newtrialPars)

up_session :: TheData -> BigPar -> Sampler BigPar
up_session thedata ((popmeanssds, trialsds, betas), sessmeans, sessbetas, trialPars) = do
  newsessmeans <- sampleMany $ forIdx sessmeans $ \sess -> 
                     metSampleP "sessmean" (\sessmean-> 
                       (sum $ for (zip (map snd $ thedata!!sess) $ map unP $ trialPars!!sess) $ 
                                \(lov, tpar)-> p_ij_i (zipWith (+) sessmean 
                                                                   (map (lov*) (unP $ sessbetas!!sess))) 
                                                      (map unP trialsds) tpar) +
                          p_i_pop (map (fst . unP) popmeanssds) 
                                  (map (snd . unP) popmeanssds) sessmean) (sessmeans!!sess)
  newsessbetas <- sampleMany $ forIdx sessbetas $ \sess -> 
                     metSampleP "sessbetas" (\sessbeta-> 
                       (sum $ for (zip (map snd $ thedata!!sess) $ map unP $ trialPars!!sess) $ 
                                \(lov, tpar)-> p_ij_i (zipWith (+) (unP $ newsessmeans!!sess)
                                                                   (map (lov*) sessbeta)) 
                                                      (map unP trialsds) tpar)  + 
                       p_i_pop (map (fst . unP) betas) 
                               (map (snd . unP) betas) sessbeta ) (sessbetas!!sess)
  return ((popmeanssds, trialsds, betas), newsessmeans, newsessbetas, trialPars)

up_pop :: TheData -> BigPar -> Sampler BigPar
up_pop thedata ((popmeanssds, trialsds, betas), sessmeans, sessbetas, trialPars) = do
  newpopmeanssds <- sampleMany $ forIdx (popmeanssds) $ \msi->
                        metSampleP "popmeans" (\(pm, ps)-> let pmean = setIdx msi pm $ map (fst. unP) popmeanssds 
                                                               psd = setIdx msi ps $ map (snd . unP) popmeanssds  in
                                                           (sum $ for sessmeans $ p_i_pop pmean psd . unP ) + 
                                                           ling ps ) $ popmeanssds!!msi
  newbetas <- sampleMany $ forIdx (betas) $ \bi->
                        metSampleP "popbetas" (\(bm, bs)-> let bmean = setIdx bi bm $ map (fst. unP) betas 
                                                               bsd = setIdx bi bs $ map (snd . unP) betas  in
                                                           (sum $ for sessbetas $ p_i_pop bmean bsd . unP ) + 
                                                           ling bs ) $ betas!!bi
  newtrialsds <- sampleMany $ forIdx trialsds $ \si -> 
                        metSampleP "trialsds" (\sd-> (sum $ map sum $ forIdx2 trialPars $ \sess tr->
                                       let trsds = setIdx si sd $ map unP trialsds 
                                           lov = snd $ thedata!!sess!!tr in
                                       p_ij_i (zipWith (+) (unP $ sessmeans!!sess)
                                                           (map (lov*) (unP $ sessbetas!!sess))) 
                                              trsds 
                                              (unP $ trialPars!!sess!!tr)) +
                                       (ling sd)
                                   ) (trialsds!!si)
  return ((newpopmeanssds, newtrialsds, newbetas), sessmeans, sessbetas, trialPars)

p_ij_i means sds pars =  sum $ map (\(mu, sd, x) ->  P.logGaussD mu sd x) $ zip3 means sds pars

p_i_pop = p_ij_i

p_pop _ sds = sum $ map ling sds

ling = const 1 --log . invGammaD 0.001 0.001


likelihoodH1 spikes pars@[amp, t0, tau1, tau2, tau3, pslow] =
--    likelihood (realToFrac rate) (realToFrac tau) (realToFrac baseline) (realToFrac t0) 
    (U.sum $ (U.map (log. r amp t0 tau1 tau2 tau3 pslow ) spikes))- (integralR pars 6 - integralR pars 0)

gibbsSF ::TheData -> StochFun BigPar BigPar 
gibbsSF thedata = condSampler (up_trial thedata) >>> condSampler (up_session thedata) >>> condSampler (up_pop thedata )

--[amp, t0, tau1, tau2, tau3, pslow, baseline]


--get ML trialpars
-- 1. transform spikes to histograms
-- 2. fit

trialParSampler = uniform 0.1 1 >>= \t1 -> 
                  return [100, 5, t1, 0.5, 2, -2]

penalty_tau1 [amp, t0, tau1, tau2, tau3, pslow] 
    | tau1 < 1e-2 = 5000000*abs (1e-2 - tau1)
    | otherwise = 0
            
penalty_pslow [amp, t0, tau1, tau2, tau3, pslow] 
    | pslow < 0 = 5000000*abs (pslow)
    | pslow > 1 = 5000000*abs (pslow-1)
    | otherwise = 0


penalty p = penalty_tau1 p -- + penalty_pslow p                                 

mlTrialPars :: [[(U.Vector Double, Double)]] -> [[(TrialPar, Double)]]
mlTrialPars thedata = for2 thedata f
    where f (spks, lov) = 
              let histo = head $ histManyOver [((0,6),())] 0.05 $ map (flip (,) ()) $ U.toList spks
                  (pars, _, err) = fitG' (rFromPars) trialParSampler penalty histo
              in (pars, lov)

--err in range 4000-17000

mlSessBetasMean :: [[(TrialPar,Double)]] -> [(TrialPar,TrialPar)]
mlSessBetasMean thepars = map sessf thepars
    where sessf :: [(TrialPar,Double)] -> (TrialPar,TrialPar)
          sessf sdata =
              let sdata' :: [[(Double, Double)]]
                  sdata' = for sdata $ \(tpars,lov) -> zip (repeat lov) tpars 
                  foo = map (zip (map snd sdata)) $ transpose $ map fst sdata
              in unzip $ map (runStat regressF) foo


{-mlSessBetasMean :: [[(TrialPar,Double)]] -> [(TrialPar,TrialPar)]
mlSessBetasMean thepars = map sessf thepars
    where sessf :: [(TrialPar,Double)] -> (TrialPar,TrialPar)
          sessf sdata =
              let sdata' :: [[(Double, Double)]]
                  sdata' = for sdata $ \(tpars,lov) -> zip (repeat lov) tpars 
                  --foo = transpose $ map fst sdata
              in unzip $ runStatOnMany regressF $ sdata' -}

{-mlTrialSDs :: [[(TrialPar,Double)]] -> [(TrialPar,TrialPar)] -> TrialPar
mlTrialSDs thepars betameans = avgit $ map f $ zip thepars betameans
    where avgit :: [TrialPar] -> TrialPar
          avgit bysess = map (sqrt . runStat (before meanF square)) $ transpose bysess
          f :: ([(TrialPar,Double)], (TrialPar,TrialPar)) -> TrialPar
          f (sessparlovs, (betas, alphas)) = 
              map (runStat stdDevF)
              $ for sessparlovs $ \(tpars, lov)-> map (\(p,b, a)-> p-(a+b*lov) ) $ zip3 tpars alphas betas -}

mlTrialSDs :: [[(TrialPar,Double)]] -> [(TrialPar,TrialPar)] -> TrialPar
mlTrialSDs thepars betameans = avgit $ map f $ zip thepars betameans
    where avgit :: [TrialPar] -> TrialPar
          avgit bysess = map (sqrt . runStat (before meanF square)) $ transpose bysess
          f :: ([(TrialPar,Double)], (TrialPar,TrialPar)) -> TrialPar
          f (trialparlovs, (betas, alphas)) = 
              map (runStat stdDevF) $ transpose 
              $ for trialparlovs $ \(tpars, lov)-> map (\(p,b,a)-> p-(a+b*lov) ) $ zip3 tpars betas alphas


mlPopMeans :: [(TrialPar,TrialPar)] -> TrialPar
mlPopMeans betameans = map (runStat meanF) $ transpose $ map snd betameans
mlPopSds betameans = map (runStat stdDevF) $ transpose $ map snd betameans
mlBetaMeans betameans = map (runStat meanF) $ transpose $ map fst betameans
mlBetaSds betameans = map (runStat stdDevF) $ transpose $ map fst betameans


mlPars :: [[(U.Vector Double, Double)]] -> BigPar
mlPars thedata = 
  let trialpars = mlTrialPars thedata
      mlSess = mlSessBetasMean trialpars 
      popmeans = mlPopMeans mlSess
      popsds = mlPopSds mlSess
      betameans = mlBetaMeans mlSess
      betasds = mlBetaSds mlSess
      trialsds = mlTrialSDs trialpars mlSess
      sessmeans = map snd mlSess
      sessbetas = map fst mlSess 
  in ((map newParam $ zip popmeans popsds, map newParam trialsds, map newParam $ zip betameans betasds), 
               map newParam sessmeans, map newParam sessbetas, map (map (newParam . fst)) trialpars)

priorSamplerG :: Int -> [Int] -> Sampler BigPar
priorSamplerG nsess ntrialsPerSess= 
    let k = 1.01 in 
    do popmeans <- mapM2 uniform (map (/k) fixPars) (map (*k) fixPars)
       popsds <- mapM2 uniform (map (/k) fixPopSds) (map (*k) fixPopSds)

       betameans <- mapM2 uniform (map (negate . (/10)) fixPars) (map (/10) fixPars)
       betasds <- mapM2 uniform (map (const 0 ) fixPopSds) (map (/100) fixPopSds)       
       trialsds <- mapM2 uniform (map (/k) fixPopSds) (map (*k) fixPopSds)

       sessmeans <- times nsess $ mapM2 gauss popmeans popsds
       sessbetas <- times nsess $ mapM2 gauss betameans betasds
       trialpars <- forM ntrialsPerSess $ \ntrs -> (times ntrs $ mapM2 gauss popmeans popsds)
       return ((map newParam $ zip popmeans popsds, map newParam trialsds, map newParam $ zip betameans betasds), 
               map newParam sessmeans, map newParam sessbetas, map (map newParam) trialpars)

chopData2 :: (ChopByDur obs,Shiftable obs) => [Duration [Int]] -> obs -> [[obs]]
chopData2 durs allspikes = 
    let sessInts = nub $ map (head . snd) durs
    in for sessInts $ \i-> chopAndReset ((!!1) <$$> (((==i) .head)//durs)) allspikes

lrsq = log . recip . (\x-> x*x)



ofInterest :: BigPar -> [Double]
ofInterest ((popmeanssds, trialsds, betas), sessmeans, sessbetas, trialPars) = 
    (map (fst . unP) popmeanssds) ++ (map (snd . unP) popmeanssds) ++ (map (fst . unP) betas) ++ 
    (map (snd . unP) betas)++ (map unP trialsds) ++ (unP $ head sessmeans) ++( unP $ head $ head trialPars)

-- ++popsds -- ++trialsds

parNames = words $ "amp t0 tau1 tau2 tau3 pslow "++
                   "ampsd t0sd tau1sd tau2sd tau3sd pslowsd "++
                   "ampbeta t0beta tau1beta tau2beta tau3beta pslowbeta "++
                   "ampbetasd t0betasd tau1betasd tau2betasd tau3betasd pslowbetasd "++
                   "amptrsd t0trsd tau1trsd tau2trsd tau3trsd pslowtrsd "++
                   "amps1mean t0s1mean tau1s1mean tau2s1mean tau3s1mean pslows1mean "++ 
                   "amps1tr1 t0s1tr1 tau1s1tr1 tau2s1tr1 tau3s1tr1 pslows1tr1"



help = do
  putStrLn "LovSpikes {niters} {chain name} {chain number}"

main = do
  nargs <- length `fmap` getArgs
  case nargs of 
    3 -> main3
    _ -> help

main3 = do
  (read -> count::Int) : filenm : (read -> threadn::Int) :_  <- getArgs 
  --let dropn = (count*3) `div` 4
  --putStrLn $ "droping "++show dropn
  (concat -> spikes, 
   concat  -> running, 
   concat  -> sess, 
   concat -> approachLoV) <- fmap unzip4 $ manySessionData $ do
           spikes <-  map fst `fmap` events "spike" ()
           running <- durations "running" ()
           modNm <- durations "moduleName" "foo"
           approachLoV <- extendDur 1 `fmap` durations "approachLoV" (1::Double)
           sess <- sessionDur
           whenMaybe (not . null $ (=="simulatePoissonSpikes")//modNm) $ 
                     return (spikes, running, sess, approachLoV) 
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
      thelovs = map (map $ snd . head) $ chopData2 segs $ (\lov-> log (lov / 0.02) / log 2) <$$> approachLoV
      (thedata::TheData) = zipWith (zip) thespikes thelovs

  let acceptInit :: BigPar -> Bool
      acceptInit pars = all notNanInf 
                        $ map (uncurry likelihoodH1) 
                        $ zip2d (map (map fst) thedata) 
                                (map ( map unP ) $ fth4 pars)
  {-inits <- fmap (filter acceptInit) 
           $ runSamplerIO 
           $ priorSamplerG (length sess) 
                           (map (length . (`during` running) . (:[])) sess) -}


  let inits = [mlPars thedata]

  writeFile (filenm++"_parnames.mcmc") $ show parNames 
                            -- , "trialRateSD", "tau", "baseline", "t0"]
  --inPar nthreads $ \threadn-> do

  putStrLn "fitting trial pars"



  do
    let baymarkov = Mrkv (gibbsSF thedata) (head inits) id
    putStrLn "initials: " 
    mapM print $ zip parNames $ ofInterest (head inits)
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



