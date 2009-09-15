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
         loom <- compileFile "Loom" [("lov",realT)]
         forever $ do pause $ 8*60
                      forM [1..30] $ \i -> do
                         pause 60
                         invoke loom [("lov", 0.04)]
                         inLast ("repetition" := idInt i)

