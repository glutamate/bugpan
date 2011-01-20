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
  deleteSessionIfExists "adaexpif"
  inApproxSession "new:adaexpif" $ do
    intfire <- useFile "AdaExpIf" 
                         (realTs "rate") []
    --io $ print intfire
    {-intfireStep <- useFile "IntfireStep" 
                         (realTs "stepAmp") [] -}

    --determineS intfireStep [("stepAmp", return 6.05e-10)]

--    times 5 $ do 
{-    determineS intfire 
                 [("rate", return 0)] -}
    determineS intfire 
                 [("rate", return 400)]