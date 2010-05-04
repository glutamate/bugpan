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

amptrialsd = 10
amppopmean = 210
amppopsd = 10

baselinemean = 0.01
baselinesd = 0.001


tau1popmean = 6.83e-5
tau1popsd = 6.83e-6
tau1sd = 6.83e-6

tau2popmean = 0.27
tau2popsd = 0.02
tau2sd = 0.002

tau3popmean = 1.07
tau3popsd = 0.1
tau3sd = 0.1

t0sd = 0.01
t0popmean = 4.9
t0popsd = 0.01

pslowpopmean = 0.15
pslowpopsd  = 0.02
pslowsd = 0.01

betaampmean = -50
betaampsd   = 0.001

betat0mean = -0.05
betat0sd   = 0.001

betatau1mean = 0
betatau1sd   = 0.00000000001

betatau2mean = 0.08
betatau2sd   = 0.02

betatau3mean = 0
betatau3sd   = 0.001

betapslowmean = 0.05
betapslowsd   = 0.001



realTs nms = zip (words nms) $ repeat realT

main = do
 deleteSessionIfExists $ "intfire"
 inApproxSession ("new:intfire") $ do          
    simspikes <- useFile "Intfire" 
                         (realTs "amp t0 tau1 tau2 tau3 pslow baseline lov") []

    ampsessmean <- sampleQ $ gauss amppopmean amppopsd
    baseline <- sampleQ $ gauss baselinemean baselinesd
    tau1mean <- sampleQ $ gauss tau1popmean tau1popsd
    tau2mean <- sampleQ $ gauss tau2popmean tau2popsd
    tau3mean <- sampleQ $ gauss tau3popmean tau3popsd

    t0mean <- sampleQ $ gauss t0popmean t0popsd
    pslowmean <- sampleQ $ gauss pslowpopmean pslowpopsd
            
    betaamp <- sampleQ $ gauss betaampmean betaampsd
    betat0 <- sampleQ $ gauss betat0mean betat0sd
    betatau1 <- sampleQ $ gauss betatau1mean betatau1sd
    betatau2 <- sampleQ $ gauss betatau2mean betatau2sd
    betatau3 <- sampleQ $ gauss betatau3mean betatau3sd
    betapslow <- sampleQ $ gauss betapslowmean betapslowsd

    ntrials <- sampleQ $ oneOf [50..200]

    times ntrials $ do 
      --lov <- sampleQ $ oneOf [0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05]
      lov <- sampleQ $ oneOf [0.01, 0.02, 0.04]
      let lv = log (lov / 0.02) / log 2
      determineS simspikes 
                 [("amp", gauss (ampsessmean+lv*betaamp) amptrialsd),
                  ("t0", gauss (t0mean+lv*betat0) t0sd),
                  ("tau1", gauss (tau1mean+lv*betatau1) tau1sd),
                  ("tau2", gauss (tau2mean+lv*betatau2) tau2sd),
                  ("tau3", gauss (tau3mean+lv*betatau3) tau3sd),
                  ("pslow", gauss (pslowmean+lv*betapslow) pslowsd),
                  ("baseline", return baseline),
                  ("lov", return lov)]
            

