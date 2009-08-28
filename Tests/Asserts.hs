{-# LANGUAGE BangPatterns #-}

module Tests.Asserts where

import System.Time
import Database
import Parse
import ImpInterpret
import EvalM hiding (ListT)
import Eval
import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans
import Control.Monad.State.Lazy
import HaskSyntaxUntyped
--import QueryUnsafe
import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
import Math.Probably.PlotR
import QueryRun
import ValueIO
import Numbers


sigStartBetween str rng (Signal t1 _ _ _) =
    assertBetween str rng t1
sigEndBetween str rng (Signal _ t2 _ _) =
    assertBetween str rng t2
sigDtBetween str rng (Signal t1 _ dt _) =
    assertBetween str rng dt

assertBetween str (lo, hi) x  = 
    if x>lo && x < hi
       then liftIO . putStrLn $ str++ " ok"
       else liftIO . putStrLn $ str ++" FAIL: not "++show lo++"<"++show x++"<"++show hi
assertEqual str x y = 
    if x==y 
       then liftIO . putStrLn $ str++ " ok"
       else liftIO . putStrLn $ str ++" FAIL: not "++show x++"=="++show y

assertTagsBetween str rng tgs = do
  let tags = map getTag tgs 
  forM tags $ \val -> assertBetween str rng val
assertTagsEqual str x tgs = do
  let tags = map getTag tgs 
  forM tags $ \val -> assertEqual str x val

assertEvTimesBtw str rng tgs = do
  let tms = map fst tgs 
  forM tms $ \tm -> assertBetween str rng tm
              
