module Main where

import Query
import Driver
import Expr
import EvalM

main = do
  deleteSessionIfExists "rttest"
  inApproxSession "new:rttest" $ do
        tok <- useRT "TestSim" [("rate", NumT (Just RealT))]
        invokeRT tok [("rate", 100)]