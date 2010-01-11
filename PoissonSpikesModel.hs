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

likelihood (rate, tau, baseline, t0) spikes =
    undefined

manyLikeOver :: ChopByDur obs => [Duration a] -> (theta -> P.PDF obs) -> (obs -> P.PDF theta)
manyLikeOver durs lh1 = \obs-> \theta->  product $ map (lh1 theta) $ chopByDur durs obs


main = 
    do inSessionFromArgs $ do          
         spike <- events "spike" ()
         running <- durations "running" ()
         bsam <- io $ bayes 10000 (manyLikeOver running likelihood spike) priorSampler
         ps <- take 1000 `fmap` (io $ runSamplerIO bsam)
         io $ print $ meanSDF `runStat` (map (\(rate, tau, baseline, t0)->rate) ps)

         return ()
