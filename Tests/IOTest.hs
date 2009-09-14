{-# LANGUAGE BangPatterns #-}

module Main where

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
import Tests.Asserts
import TNUtils

main = ioTest


real = double

ioTest = inTemporarySession $ do
           prog <- use "TestStore"
           runFrom prog 0 
           runFrom prog 5 
           runFrom prog 10

           storeAs "storeEvent" [(2.3::Double, ()), (2.4, ())]
           storeAs "storeIntDur" [((2.3::Double, 3.3::Double), 4::Int), ((4.1,4.9), 5)]
           --storeAs "storeUnitDur" [((2.3::Double, 3.3::Double), ()), ((4.1,4.9), ())]
           --storeAs "storeDblDur" [((2.3::Double, 3.3::Double), 4::Double), ((4.1,4.9), 5)]
          
           secs <- signalsDirect "secs"
           anEvent <- events "anEvent" ()
           aNumEvent <- events "aNumEvent" real
           aStringDur <- durations "aStringDur" ""
           aPairDur <- durations "aPairDur" (real, ()) 
           storeEvent <- events "storeEvent" ()
           storeIntDur <- durations "storeIntDur" (1::Int)
           storeUnitDur <- durations "storeUnitDur" ()

           assertTagsBetween "secs stdDev" (0.28,0.30) $ sigStat stdDevF secs
           assertEqual "#secs" 3 $ length secs
           assertEqual "#events" 3 $ length anEvent
           assertEqual "#storeEvents" 2 $ length storeEvent
           assertEqual "#storeIntDurs" 2 $ length storeIntDur
           assertEqual "#storeUnitDurs" 2 $ length storeUnitDur
           assertEqual "#anumevent" 6 $ length aNumEvent
           assertEqual "#strdur" 3 $ length aStringDur
           assertEqual "#pairdur" 6 $ length aPairDur


           assertEvTimesBtw "anEvent time (2/3 fail)" (0.29,0.31) $ anEvent
           assertEvTimesBtw "storeEvent time" (2.2,2.5) $ storeEvent
           assertTagsEqual "anEvent tag" () anEvent
           assertTagsEqual "storeEvent tag" () storeEvent
           assertTagsBetween "aNumEvent tag" (4.5, 6.6) aNumEvent
           assertTagsBetween "storeIntDur tag" (3, 6) storeIntDur
                            
           assertTagsEqual "aStringDur val" "foo"  aStringDur

           assertTagsEqual "pairdur unit tag" () (snd <$$> aPairDur)
           assertTagsBetween "pairdur num tag" (0.5,2.5) (fst <$$> aPairDur) 

           sigStartBetween "secs start" (4.9,5.1) $ secs !!1
           sigEndBetween "secs stop" (5.9,6.1) $ secs !!1
           sigDtBetween "secs dt" (0.0001, 0.01) $ secs !!1
   
           Session bdir _ <- getSession
           liftIO $ do t<- fileTypeTag $ bdir ./ "durations/storeIntDur/stored2"
                       print t