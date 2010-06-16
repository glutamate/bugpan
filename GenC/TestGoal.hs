module Main where

import Query
import Driver

main = do
  deleteSessionIfExists "rttest"
  inApproxSession "new:rttest" $ do
        tok <- useRT "Test" []
        invokeRT tok []