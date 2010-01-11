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
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import QueryTypes
import Math.Probably.FoldingStats


priorSampler = 
    do rate <- uniform 0 400
       tau <- uniform 0.01 0.3
       baseline <- uniform 0 10
       t0 <- uniform 4 6
       return (rate, tau, baseline, t0)

likelihood pars@(rate, tau, baseline, t0) spikes =
    exp (integralR pars 6) * product (map (r pars . fst) spikes)

only_pos x = if x>0 then x else 0

r (rate, tau, baseline, t0) t 
    = only_pos((-(t-t0)/tau)*exp(1+(t-t0)/tau))*(rate-baseline)+baseline
--http://integrals.wolfram.com/index.jsp?expr=(-(x-z)%2Ft)*Exp[1%2B(x-z)%2Ft]*(r-b)%2Bb&random=false

integralR pars@(rate, tau, baseline, t0) t 
    | t>t0 = integralR pars t0 + baseline * t
    | otherwise = (baseline - rate)*exp((tau+t-t0)/tau)*(-tau+t-t0)+baseline*t

manyLikeOver :: ChopByDur obs => [Duration a] -> (theta -> P.PDF obs) -> (obs -> P.PDF theta)
manyLikeOver durs lh1 = \obs-> \theta->  product $ map (lh1 theta) $ chopByDur durs obs


main = 
    do inSessionFromArgs $ do          
         spike <- events "spike" ()
         running <- durations "running" ()
         bsam <- io $ bayes 100000 (manyLikeOver running likelihood spike) priorSampler
         ps <- take 10000 `fmap` (io $ runSamplerIO bsam)
         io $ putStrLn $ "rate "++ show ((meanSDF `both` nSumSumSqr) `runStat` map fst4 ps) 
         io $ putStrLn $ "tau "++ show ((meanSDF `both` nSumSumSqr) `runStat` map snd4 ps)
         io $ putStrLn $ "baseline "++ show ((meanSDF `both` nSumSumSqr) `runStat` map trd4 ps)
         io $ putStrLn $ "t0 "++ show ((meanSDF `both` nSumSumSqr) `runStat` map fth4 ps)

         return ()
