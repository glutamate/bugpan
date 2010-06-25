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
import Control.Monad
import Numbers

realTs nms = zip (words nms) $ repeat (NumT (Just RealT))

gampa = 2e-9
gmaxk = 2e-7

main = do
  inApproxSession "new:ff3" $ do
        tok <- useRT "SynACurrent" $ realTs "rate gmaxk gampa"
        let runIt r gk ga = do
                 invokeRT tok [("rate", r),("gmaxk", gk), ("gampa", ga)]
                 inLast ("rate" := r)
                 inLast ("gampa" := ga)
                 inLast ("gmaxk" := gk)           
                 wait 1
        forM_ [10,20..600] $ \r -> do
           runIt (NumV $ NReal r) gmaxk gampa
           runIt (NumV $ NReal r) 0 gampa
 
