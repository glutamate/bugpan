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
         intfire <- useFile "Intfire" [("_tmax", realT),
                                       ("rate", realT),
                                       ("tonicInhib", realT)]
         10 `times` determine intfire [("_tmax", always 4),
                                       ("rate", uniform 600 1000),
                                       ("tonicInhib", oneOf [0, 1])]


