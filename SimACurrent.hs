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
import PlotGnuplot
import QueryPlots
import NewSignal

{-

ghc --make SimACurrent -DNODAQ && ./SimACurrent

-}

realTs nms = zip (words nms) $ repeat realT

main = do
  deleteSessionIfExists "acurrent"
  inApproxSession "new:acurrent" $ do
    step <- useFile "ACurrentStep" 
                         (realTs "stepAmp gmaxk rate oneSpikeAmp") []
    let runIt (stepAmp, gmaxk, rate, oneSpikeAmp) = do
            determineS step [("stepAmp", return stepAmp),
                             ("gmaxk",return gmaxk),
                             ("rate",return rate), 
                             ("oneSpikeAmp",return oneSpikeAmp)]
            inLast ("stepAmp" := stepAmp)
            inLast ("gmaxk" := gmaxk)
            inLast ("rate" := rate)
            inLast ("oneSpikeAmp" := oneSpikeAmp)
    runIt (2e-10, 1e-8, 0, 0) --step, acurrent
    runIt (2e-10, 0, 0, 0) --step, no acurrent
    runIt (0, 1e-7, 0, 1.0e-12) --onesyn, acurrent
    runIt (0, 0, 0, 1.0e-12) --onesyn, no acurrent
    forM_ [10,20..600] $ \r -> do
         runIt (0, 0, r, 0)
         runIt (0, 1e-7, r, 0)

  {-inApproxSession "acurrent" $ do
         vm <- signalsDirect "vm"
         act <- signalsDirect "a"
         inact <- signalsDirect "b"
         ika <- signalsDirect "ika"
         let t0 = sigT1 $ head ika
         let showDur = [((t0, t0+0.1), ())]
         io $ gnuplotOnScreen $ alignBy sigStart vm -}

-- alignBy sigStart vm --during showDur vm :==: during showDur ika


    

  