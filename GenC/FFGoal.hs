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

main = do
  inApproxSession "new:fftest12" $ do
        tok <- useRT "SynACurrent" $ realTs "rate gmaxk gampa"
        let runIt r gk ga = do
                 invokeRT tok [("rate", r),("gmaxk", gk), ("gampa", ga)]
                 inLast ("rate" := r)
                 inLast ("gampa" := ga)
                 inLast ("gmaxk" := gk)           
                 wait 10
        forM_ [20,30..200] $ \r -> do
           runIt (NumV $ NReal r) 2e-8 gampa
           runIt (NumV $ NReal r) 1e-8 gampa
           runIt (NumV $ NReal r) 0 gampa
 
