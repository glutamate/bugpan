{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables #-}
module Main where

import Math.Probably.Sampler
import Math.Probably.StochFun
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import Control.Monad
import TNUtils
import Control.Arrow


import StatsModel


mualpha = 1
sdalpha = 0.2

mubeta = 3
sdbeta = 0.5

trialsd = 0.3

metSampleP = metSample1P depSam
metSamplePx0 x0  = metSample1P (depSamx0 x0)
metSamplePCL = metSample1PCL depSam

depSam w x0 =  mutGaussAbs x0 $ w*0.005
depSamx0 x0 w _ =  mutGaussAbs x0 $ w*0.005


simGroup = do
  nobs <- oneOf [20..100]
  alpha <- gaussD mualpha sdalpha
  beta <- gaussD mubeta sdbeta
  forM [0..nobs-1] $ const $ do
             x <- uniform 0 1
             y <- gauss (alpha+beta*x) trialsd
             return (x,y)

lh ([alpha, beta], trialsd) (x,y) = P.logGaussD (alpha+beta*x) trialsd y

type GroupPar = [Double]
type BigPar = (Param (GroupPar, GroupPar), Param Double, [Param GroupPar])


initSam :: Int -> [Int] -> Sampler BigPar
initSam nsess ntrialsPerSess = do
  meanalpha <- uniform 0 5
  meanbeta <- uniform 0 5
  let means = [meanalpha, meanbeta]
  sdalpha <- uniform 0 0.5
  sdbeta <- uniform 0 0.5
  let sds = [sdalpha, sdbeta]
  trialsd <- uniform 0 1
  trpars <- times nsess $ mapM2 gauss means sds
  return (newParam (means,sds), newParam trialsd, map newParam trpars)
  


prtolist (x,y) = [x,y]
up_groups :: [[(Double,Double)]] -> BigPar -> Sampler BigPar
up_groups thedata ((popmeanssds), trialsd, groupPars) = do
  newGroups <- sampleMany $ forIdx groupPars $ \(gi)-> metSampleP (\(gp)-> 
                                     (sum $ for (thedata!!gi) $ lh (gp, unP trialsd)) + 
                                     p_ij_i (fst $ unP popmeanssds) (snd $ unP popmeanssds) gp
                                  ) (groupPars!!gi)
  return ((popmeanssds), trialsd, newGroups)

up_trialsd :: [[(Double,Double)]] -> BigPar -> Sampler BigPar
up_trialsd thedata ((popmeanssds), trialsd, groupPars) = do
  newtrialsd <- metSampleP (\trsds-> (sum $ forIdx groupPars $ \gi-> 
                                         sum $ for (thedata!!gi) $ lh (unP (groupPars!!gi), trsds))+
                                     lrsq trsds
                                      ) trialsd
  return (popmeanssds, newtrialsd, groupPars)

up_popmeans :: BigPar -> Sampler BigPar
up_popmeans ((popmeanssds), trialsd, groupPars) = do
  newpopmeans <- metSampleP (\(pmean,psd) -> (sum $ for groupPars $ p_ij_i (pmean) (psd) . unP) +
                                     sum (map lrsq psd)
                            ) popmeanssds
  return (newpopmeans, trialsd, groupPars)

gibbs thedata = condSampler (up_groups thedata) >>> condSampler (up_trialsd thedata) >>> condSampler (up_popmeans )




p_ij_i means sds pars =  sum $ map (\(mu, sd, x) ->  P.logGaussD mu sd x) $ zip3 means sds pars

p_i_pop = p_ij_i

p_pop _ sds = sum $ map (lrsq) sds

lrsq = log . recip. (\x->x*x)

main = do
  thedata <- take 10 `fmap` (runSamplerIO simGroup)  
  let nsess = length thedata
      ntrpersess = map length thedata
  initv <- fmap head $ runSamplerIO $ initSam nsess ntrpersess
  let baymarkov = Mrkv (gibbs thedata) initv id
  ps <- take 10000 `fmap` runMarkovIO baymarkov

  let (meansds, trsd, _)  = last ps
  print2 "means " $ fst $ unP meansds
  print2 "sds " $ snd $ unP meansds
  print2 "trsds " $ unP trsd


  return ()




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


mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = mapM (uncurry f) $ zip xs ys
