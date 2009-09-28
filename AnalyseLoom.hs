{-# LANGUAGE BangPatterns #-}

module Main where

--import System.Time
--import Database
--import Parse
--import EvalM hiding (ListT)
--import Eval
--import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans
--import Control.Monad.State.Lazy
--import HaskSyntaxUntyped
--import QueryUnsafe
--import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
--import ValueIO
--import Numbers
import TNUtils
import PlotGnuplot
import QueryPlots
import Data.List hiding (groupBy)
import Data.Ord
import System.Environment
import Database

main = loomAnal

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

                                       



loomAnal = do ress <- inEverySession $ do  
                        snm <- (last . splitBy '/' . baseDir) `fmap` getSession
                        rep <- durations "repetition" (1::Int)
                        rearLoc <- durations "rearingLocation" ""
                        spikes <- events "spikes" ()
                        let numSpks = (fst . snd) `fmap` habitAnal rep spikes
                        return $ ((take 4 snm, snd . head $ rearLoc), numSpks)
              let (up, down) = partition ((=="upstairs") . snd. fst) ress
              
              let plotUp = foldl (.+.) (GnuplotBox Noplot) $ map plLin up
              let plotDown = foldl (.+.) (GnuplotBox Noplot) $ map plDash down
              gnuplotToPNG "habit.png" $ plotUp :+: plotDown



    where plLin ((snm, loc), spks) =  (snm ++ " "++loc, Lines (zip wholeReals spks))
          plDash ((snm, loc), spks) =  (snm ++ " "++loc, Dashed (zip wholeReals spks))
          x .+. y = GnuplotBox $ (x) :+: (y)


wholeReals = [(1::Double)..]