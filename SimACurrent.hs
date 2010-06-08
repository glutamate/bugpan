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

{-

ghc --make SimACurrent -DNODAQ && ./SimACurrent

-}

realTs nms = zip (words nms) $ repeat realT

main = do
  deleteSessionIfExists "acurrent"
  inApproxSession "new:acurrent" $ do
    step <- useFile "ACurrentStep" 
                         (realTs "stepAmp gmaxk") []

    determineS step [("stepAmp", return 6.05e-10),
                     ("gmaxk",return 1e-7)]

    determineS step [("stepAmp", return 6.05e-10),
                     ("gmaxk",return 0)]
  inApproxSession "acurrent" $ do
         vm <- signalsDirect "vm"
         act <- signalsDirect "act"
         inact <- signalsDirect "inact"
         io $ gnuplotOnScreen $ alignBy sigStart vm 
    

  