module Main where

import QueryRun
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

main = 
    do getArgs >>= print
       inSessionFromArgs $ do          
         intfire <- useFile "SimulatePoissonSpikes" [] []
         --100 `times` determine intfire []
         return ()


