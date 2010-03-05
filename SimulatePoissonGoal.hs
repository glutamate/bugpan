module Main where

import QueryRun hiding (uniform,oneOf)
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
import Control.Monad.Trans
import QueryUtils
import QueryTypes

amptrialsd = 40
amppopmean = 800
amppopsd = 40

baselinemean = 0.2
baselinesd = 0.1

tau1sd = 0.1
tau2sd = 0.1
tau3sd = 0.05

tau1popmean = 0.3
tau1popsd = 0.05

tau2popmean = 0.5
tau2popsd = 0.1

tau3popmean = 0.2
tau3popsd = 0.02

t0sd = 0.01
pslowsd = 0.01

t0popmean = 5
t0popsd = 0.01

pslowpopmean = 0.1
pslowpopsd = 0.02

realTs nms = zip (words nms) $ repeat realT

main = forM_ [0..9] $ \i -> do
 deleteSessionIfExists $ "poisson"++show i
 inApproxSession ("new:poisson"++show i) $ do          
    simspikes <- useFile "SimulatePoissonSpikes" 
                         (realTs "amp t0 tau1 tau2 tau3 pslow baseline") []

    ampsessmean <- sampleQ $ gauss amppopmean amppopsd
    baseline <- sampleQ $ gauss baselinemean baselinesd
    tau1mean <- sampleQ $ gauss tau1popmean tau1popsd
    tau2mean <- sampleQ $ gauss tau2popmean tau2popsd
    tau3mean <- sampleQ $ gauss tau3popmean tau3popsd

    t0mean <- sampleQ $ gauss t0popmean t0popsd
    pslowmean <- sampleQ $ gauss pslowpopmean pslowpopsd
            
    ntrials <- sampleQ $ oneOf [20..100]

    times ntrials $ do 
      determineS simspikes 
                 [("amp", gauss ampsessmean amptrialsd),
                  ("t0", gauss t0mean t0sd),
                  ("tau1", gauss tau1mean tau1sd),
                  ("tau2", gauss tau2mean tau2sd),
                  ("tau3", gauss tau3mean tau3sd),
                  ("pslow", gauss pslowmean pslowsd),
                  ("baseline", return baseline)]
            

