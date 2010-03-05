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
import qualified Data.Array.Vector.UArr as UA
import Data.Binary
import GHC.Conc
import StatsModel
import Foreign.Storable
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Arrow
import Numeric.GSL.Minimization



forIdx ::  [a] -> (Int ->  b) ->[b]
forIdx xss f = map (f . snd) $ zip xss [0..]

forIdx2 :: [[a]] -> (Int -> Int -> b) -> [[b]]
forIdx2 xss f = map g $ zip xss [0..]
    where g (xs, i) = map  (f i . snd) $ zip xs [0..]
 
for2 :: [[a]] -> (a->b) -> [[b]]
for2 xss f = map (map f) xss 

type AllTrialPars = [[TrialPar]]
type TrialPar = [Double]
type TrialSDs = TrialPar
type SessMeans =  [TrialPar]
--type SessSDs = [TrialPar]
type PopMeansSds = (TrialPar, TrialPar)
--type PopSDs = Param TrialPar

type BigPar = ((PopMeansSds, TrialSDs), SessMeans, AllTrialPars) 

parToDbls :: BigPar -> [Double]
parToDbls (((popmeans, popsds), trialsds), sessmeans, trialPars) 
    = popmeans++popsds++trialsds++concat (sessmeans)++concat (concat trialPars)

npars = 7

unconcat :: Int -> [a] -> [[a]]
unconcat n xs = let (ys, rest) = splitAt n xs
                in case rest of 
                     [] -> [ys]
                     _ -> ys : unconcat n rest

unconcatUneven :: [Int] -> [a] -> [[a]]
unconcatUneven [] _ = []
unconcatUneven (n:ns) xs = let (ys, rest) = splitAt n xs
                           in ys : unconcatUneven ns rest

dblsToPar :: Int -> [Int] ->  [Double] -> BigPar
dblsToPar nsess nTrPerSess l= 
    let (popmeans, l1) = splitAt npars l
        (popsds, l2) = splitAt npars l1
        (trialsds, l3) = splitAt npars l2
        (catsessmeans, l4) = splitAt (npars*nsess) l3
    in (((popmeans, popsds), trialsds), unconcat npars catsessmeans, parseTrPars nsess nTrPerSess l4 )

parseTrPars 0 _ _  = []
parseTrPars nsess (nTr:nTrs) xs = 
    let (ys, rest) = splitAt (nTr*npars) xs
    in unconcat npars ys : parseTrPars (nsess-1) (nTrs) rest

forevery n = for [0..n-1]

lh :: [[UArr Double]]-> Int -> [Int] -> [Double] -> Double
lh thedata nsess nTrsPerSess parVec = 
    let (((popmeans, popsds), trialsds), sessmeans, trialPars) = dblsToPar nsess nTrsPerSess parVec 
    in negate $ (sum $ map sum $ forevery nsess $ \sess -> 
         forevery (nTrsPerSess!!sess) $ \tr-> 
          likelihoodH1 (thedata!!sess!!tr) (trialPars!!sess!!tr) + 
          p_ij_i (sessmeans!!sess) trialsds (trialPars!!sess!!tr)) +
       (sum $ forevery nsess $ \sess -> p_i_pop popmeans popsds (sessmeans!!sess)) +
       (sum $ map lrsq trialsds) + (sum $ map lrsq popsds)

--[amp, t0, tau1, tau2, tau3, pslow, baseline]
fixPars = [200, 5, 0.3, 0.5,0.3,0.1,0.1]
fixPopSds = [20, 0.01, 0.05, 0.05, 0.03, 0.03, 0.01]

allMul x = map (x*)



p_ij_i means sds pars =  sum $ map (\(mu, sd, x) ->  P.logGaussD mu sd x) $ zip3 means sds pars

p_i_pop = p_ij_i

p_pop _ sds = sum $ map lrsq sds

rFromPars :: TrialPar -> Double -> Double
rFromPars [amp, t0, tau1, tau2, tau3, pslow, baseline] = r amp t0 tau1 tau2 tau3 pslow baseline

likelihoodH1 spikes pars@[amp, t0, tau1, tau2, tau3, pslow, baseline] =
--    likelihood (realToFrac rate) (realToFrac tau) (realToFrac baseline) (realToFrac t0) 
    (sumU $ (mapU (log . r amp t0 tau1 tau2 tau3 pslow baseline) spikes))- (integralR pars 6 - integralR pars 0)
likelihoodH1 _ pars = error $ "likelihoodH1: got npars= "++show (length pars)
--gibbsSF thedata = condSampler (up_trial thedata) >>> condSampler up_session >>> condSampler (up_pop thedata )

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = mapM (uncurry f) $ zip xs ys

--[amp, t0, tau1, tau2, tau3, pslow, baseline]
priorSamplerG :: Int -> [Int] -> Sampler BigPar
priorSamplerG nsess ntrialsPerSess= 
    do popmeans <- mapM2  uniform  (map (/2) fixPars) (map (*2) fixPars)
       popsds <- mapM2 uniform (map (/2) fixPopSds) (map (*2) fixPopSds)
       trialsds <- mapM2 uniform (map (/2) fixPopSds) (map (*2) fixPopSds)
       sessmeans <- times nsess $ mapM2 gauss popmeans popsds
       trialpars <- forM ntrialsPerSess $ \ntrs -> (times ntrs $ mapM2 gauss popmeans popsds)
       return (((popmeans, popsds), trialsds), sessmeans,  trialpars)

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
integralR pars@[amp, t0, tau1, tau2, tau3, pslow, baseline] t 
    | t>t0 = integralR pars t0 + baseline * (t-t0)
    | otherwise = let bigterm tau = tau * exp ((tau1*t - t0*(tau1+tau)) /(tau1*tau)) * 
                                    ((tau1+tau)* exp (t0/tau1) - tau1 * exp (t/tau1)) / (tau1+tau) 
		  in amp * (pslow * bigterm tau3 - (pslow-1) * bigterm tau2) + baseline * t


ofInterest :: BigPar -> [Double]
ofInterest ((popmeanssds, trialsds), sessmeans, trialPars) = 
    (fst $ popmeanssds) ++ take 1 (snd $ popmeanssds) ++ take 1 ( trialsds) ++ [head $  head sessmeans] ++ [head $ head $ head trialPars] 

-- ++popsds -- ++trialsds

parNames = words "amp t0 tau1 tau2 tau3 pslow baseline ampsdpop amptrsd ampsess1 ampsess1tr1"

all2 :: (a->Bool) -> [[a]] -> Bool
all2 p = and . map (all p)

zip2d :: [[a]] -> [[b]] -> [(a,b)]
zip2d xss yss = concat $ zipWith zip xss yss

last3 [x,y,z] = [x,y,z]
last3 [] = []
last3 (x:xs) = last3 xs

main :: IO ()
main = do
  (read -> count::Int) : _  <- getArgs 
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
  let thespikes = chopData2 segs (toU spikes)
  let nthreads = numCapabilities
  putStrLn $ "splitting into nthreads="++show nthreads
  let acceptInit pars = all notNanInf $ map (uncurry likelihoodH1) $ zip2d thespikes $ trd3 pars
  let nTrPerS =  (map (length . (`during` running) . (:[])) sess)
  inits <- fmap (filter acceptInit) $ runSamplerIO $ priorSamplerG (length sess) nTrPerS
--  writeFile (filenm++"_parnames.mcmc") $ show parNames 
                            -- , "trialRateSD", "tau", "baseline", "t0"]
  inPar nthreads $ \threadn-> do
    let ini = (inits!!threadn) 
    let inivec = parToDbls ini
    print2 "initials: " $ ofInterest ini
    print2 "initial lh: " $ lh thespikes (length sess) nTrPerS inivec
    --print2 "spikes: " $ thespikes !!0 !!0
    --print2 "pars: " (head $ head $ trd3 $ inits!!threadn)
    --print2 "r(4) = " $ rFromPars (head $ head $ trd3 $ inits!!threadn) 1
    --print2 "L: " $ likelihoodH1 (thespikes!!0!!0) (head $ head $ trd3 $ inits!!threadn)
    

    let soln = minimize NMSimplex2 2E-5 count (map (/10) inivec) (lh thespikes (length sess) nTrPerS) inivec
    --gnuplotOnScreen $ (FunSeg 0 6 $ integralR fixPars) -- (head $ head $ trd3 $ inits!!threadn)
    --print $ euler 0.0001 0 6 $ rFromPars fixPars
    --print $ integralR fixPars 6

    print parNames
    print $ ofInterest $ dblsToPar (length sess) nTrPerS $ fst soln
    --writeFile ("poisson_chain"++show threadn++"lastpar.mcmc") $ show $ last ps
    --mapM print $ map ofInterest ps
    print2 "final lh: " $ lh thespikes (length sess) nTrPerS (fst soln)
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

