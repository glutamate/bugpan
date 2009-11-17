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

main = inNewSession $ do          
         intfire <- useFile "Intfire" [("rate", realT),
                                       ("tonicInhib", realT)] []
         40 `times` determine intfire [("rate", uniform 0 1200),
                                       ("tonicInhib", oneOf [0, 1e-12])]


