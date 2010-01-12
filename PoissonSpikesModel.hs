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
import Database


priorSampler = 
    do rate <- uniform 0 300
       tau <- uniform 0.01 0.3
       baseline <- uniform 0 1
       t0 <- uniform 4.5 5.5
       return (rate, tau, baseline, t0)

priorPDF (rate, tau, baseline, t0) | between 0 300 rate && between 0.01 0.3 tau &&
                                       between 0 1 baseline && between 4.5 5.5 t0 =  0
                                   | otherwise = - 1e20
    where between l u x = x > l && x < u

likelihood pars@(rate, tau, baseline, t0) spikes =
    ((sum $ (map (log . r pars . fst) spikes))- integralR pars 6  )

lhIgrl pars@(rate, tau, baseline, t0) spikes =
    exp (integralR pars 6)
lhSpks pars@(rate, tau, baseline, t0) spikes =
    exp( sum $ (map (log . r pars . fst) spikes) )

only_pos x = if x>0 then x else 0

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
    do nrate <- gauss rate 1
       ntau <- gauss tau 0.005
       nbaseline <- gauss baseline 0.01
       nt0 <- gauss t0 0.05
       return (nrate, ntau, nbaseline, nt0)

testMkv = Mrkv (condSampler proposal) (100, 0.1, 0.2, 5) id

main = 
    do inSessionFromArgs $ do          
         spike <- events "spike" ()
         running <- durations "running" ()
         ps <- io $ do
           let lh = (manyLikeOver running likelihood spike)
           let bayfun = bayesMetLog proposal (manyLikeOver running likelihood spike) priorPDF
           inits <- fmap head $ runSamplerIO priorSampler
           let baymarkov = Mrkv bayfun inits id
           ps <- (take 15000) `fmap` runMarkovIO baymarkov
         --bsam <- io $ bayes 100000 (manyLikeOver running likelihood spike) priorSampler
         --ps <- take 10000 `fmap` (io $ runSamplerIO bsam)
           putStrLn $ "inits "++show (lh inits, priorPDF inits, inits)
           let noburn = drop 8000 ps
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
           --mapM print $ map (\p-> (lh p, priorPDF p, p)) $ lastn 100 noburn 
           --tst <- take 10 `fmap` runMarkovIO testMkv
           --mapM print tst
           --return ps
         
         
         return ()

lastn n xs = let len = length xs 
             in if n > len
                   then xs
                   else drop (len - n) xs
