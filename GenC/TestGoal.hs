module Main where

import Query
import Driver
import Expr
import EvalM
import QueryUtils
import QueryTypes
import PlotGnuplot
import QueryPlots
import NewSignal

realTs nms = zip (words nms) $ repeat (NumT (Just RealT))

gampa = 2e-9
gk = 2e-7

main = do
  deleteSessionIfExists "rttest"
  inApproxSession "new:rttest" $ do
        tok <- useRT "SynACurrent" $ realTs "rate gmaxk gampa"
        invokeRT tok [("rate", 100),("gmaxk", 0), ("gampa", gampa)]
        wait 2
        invokeRT tok [("rate", 100),("gmaxk", gk), ("gampa", gampa)]
  inApproxSession "rttest" $ do
         vm <- signalsDirect "vm"
--         let t0 = sigT1 $ head vm
--         let showDur = [((t0, t0+0.1), ())]
         io $ gnuplotOnScreen $ alignBy sigStart vm 
