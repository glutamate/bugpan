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

realTs nms = zip (words nms) $ repeat realT

main = do
  inSessionFromArgs $ do
    intfire <- useFile "Intfire" 
                         (realTs "stepAmp rate") []
    determineS intfire [("stepAmp", return 1e-12),
                        ("rate", return 0)]

    times 10 $ do 
      determineS intfire 
                 [("stepAmp", return (-1))
                  ("rate", uniform 0 100)]