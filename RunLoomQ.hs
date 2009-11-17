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

main = inLastSession $ do 
         useRemoteDriver
         dplcLoom <- useFile "DisplacedLoom" [("angle", realT),
                                              ("lov", realT)] 
                                             []
         forever $ do pause 240
                      determine dplcLoom [("angle", uniform (-0.1) (0.1)),
                                          ("lov", oneOf [0.04,0.02, 0.01])]


