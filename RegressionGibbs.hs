{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables #-}
module Main where

import Math.Probably.Sampler
import Math.Probably.StochFun
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import Control.Monad
import TNUtils
import System.Environment
import Math.Probably.FoldingStats


import Control.Arrow


import StatsModel


alpha = 1

beta = 1 
trialsd = 0.3

metSampleP = metSample1P depSam
metSamplePx0 x0  = metSample1P (depSamx0 x0)
metSamplePCL = metSample1PCL depSam

depSam w x0 =  mutGaussAbs x0 $ w*0.005
depSamx0 x0 w _ =  mutGaussAbs x0 $ w*0.005


simdata = do
             x <- uniform 0 1
             y <- gauss (alpha+beta*x) trialsd
             return (x,y)

lh ((alpha, beta), trialsd) (x,y) = P.logGaussD (alpha+beta*x) trialsd y

type BigPar = (Param (Double, Double), Param Double)

initSam :: Sampler BigPar
initSam  = do
  meanalpha <- return 1 -- uniform 0 5
  meanbeta <- return 1 --uniform 0 5
  trialsd <- uniform 0 1
  return (newParam (meanalpha,meanbeta), newParam trialsd)
  

test_sampler :: Param Double -> Sampler (Param Double)
test_sampler x = do
  metSampleP (P.logGaussD 5 1) x
  

up_alphabeta :: [(Double,Double)] -> BigPar -> Sampler BigPar
up_alphabeta thedata (alphabeta , trialsd) = do
  newalphabeta <- metSampleP (\(a,b)-> 
                                     (sum $ for thedata $ lh ((a,b), unP trialsd)) + 
                                     1) alphabeta
  return (newalphabeta, trialsd)

up_trialsd :: [(Double,Double)] -> BigPar -> Sampler BigPar
up_trialsd thedata (alphabeta , trialsd) = do
  newtrialsd <- metSampleP (\trsd-> (sum $ for thedata $ lh (unP alphabeta, trsd)) + 
                                     lrsq trsd
                                      ) trialsd
  return (alphabeta, newtrialsd)

gibbs thedata = {-condSampler (up_alphabeta thedata)  >>> -}condSampler (up_trialsd thedata)

testGibbs = condSampler test_sampler


p_ij_i means sds pars =  sum $ map (\(mu, sd, x) ->  P.logGaussD mu sd x) $ zip3 means sds pars

p_i_pop = p_ij_i

p_pop _ sds = sum $ map (lrsq) sds

lrsq = log . recip. (\x->x*x)

main = do
  (read -> count::Int) : _ <- getArgs 

  thedata <- take 50 `fmap` (runSamplerIO simdata)  
  --mapM print thedata
  let nsess = length thedata

  initv <- fmap head $ runSamplerIO $ initSam 
  let baymarkov = Mrkv (gibbs thedata) initv id
  let baymarkovTest = Mrkv (testGibbs) (newParam 0.5) id
  ps <- take count `fmap` runMarkovIO baymarkov

  --let (ab, trsd)  = last ps
  --print2 "alpha,beta " $ unP ab
  --print2 "trsd " $ unP trsd
  print $ meanSDF `runStat` ( map (unP . snd) $ drop (count `div` 2) ps)

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
