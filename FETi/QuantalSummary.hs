{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction, ViewPatterns #-}
module Main where

import System.Environment
import Database
import Query hiding (io) 
import QueryTypes
import QueryUtils hiding (averageSigs)
import qualified QueryUtils as QU
import Data.Maybe
import Data.List
import Control.Monad
import System.Directory
import Math.Probably.RandIO
import QuantalHelp
import Baysig.Estimate.RTS
import Data.Binary
import qualified Numeric.LinearAlgebra as L
import Math.Probably.MCMC
import Math.Probably.FoldingStats
import System.IO
import Data.Ord
import System.Posix.Directory
import System.Cmd

import Graphics.Gnewplot.Exec
import Graphics.Gnewplot.Types
import Graphics.Gnewplot.Style
--import Graphics.Gnewplot.Panels
import Graphics.Gnewplot.Instances
import Graphics.Gnewplot.Histogram

import Control.Monad.Trans

simsum = do 
  h <- openFile ("simSummary.tex") WriteMode 
  let puts = hPutStrLn h
      plotIt nm obj = do gnuplotToPS (nm++".eps") $ obj
                         system $ "epstopdf "++nm++".eps"
                         puts $"\\includegraphics[width=16cm]{"++nm++"}"
  puts $ unlines     ["\\documentclass[11pt]{article}",
     "%include lhs2TeX.fmt",
     "%include polycode.fmt",
     "\\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}",
     "\\usepackage{graphicx}",
     "\\begin{document}"]
  forM_ [1000, 5000, 10000] $ \ntr -> do
   let ntrs = pad $ reverse $ drop 3 $ reverse $ show ntr
   pts <- fmap (concat . concat) $ forM [25::Double, 50, 100, 200, 300] $ \ns -> do
            forM [1..5] $ \run -> do
               let nsstr = take 2 $ show ns           
                   sessnm = ntrs++nsstr++show run
               vsamples::[L.Vector Double] <- fmap read $ readFile (sessnm++"/npq_samples")
               return $ zip (repeat (ns+run-3)) $ map (@>0) vsamples
   plotIt ("sim_npq_res"++ntrs) pts
  puts "\\end{document}"
  hClose h
  system $ "pdflatex .tex"
  return ()

datasess = 
 words "00c9bd 0ca3a9 22b152 512f48 7b8f60 84b41c b34863 b62b8f cf96ab fcb952"

whenM mb mu = do 
   b <- mb
   when b mu

countSigs sess = do
  putStr $ sess ++ " "
  whenM (doesFileExist (take 6 sess++"/epsps")) $ do
    lns <- fmap lines $ readFile (take 6 sess++"/epsps")
    putStrLn $ show (length lns)
  whenM (doesFileExist (take 6 sess++"/npq_samples")) $ do
    fileconts::[L.Vector Double] <- fmap read $ readFile (take 6 sess++"/npq_samples")
    
    let (mnpars, sdpars) = runStat meanSDF fileconts
    putStrLn $ show $ L.toList mnpars -- ::L.Vector Double)
    putStr $ show $ L.toList sdpars -- ::L.Vector Double)
    return ()
  putStr "\n"
  

main = do
--  simsum
  mapM_ countSigs datasess

countSigs1 sess = do
  spks <- fmap (concat . catMaybes) $ inEverySession $ whenContinues sess $ do
     rebaseRelativeTo sess
     vm <- signalsDirect "vm"
     sessionIdentifier <- getSessionName
     sessionStart <- getSessionStart
     spike <- events "spike" ()
     running <- durations "running" ()
     exclude <- durations "exclude" ()
     let swings = (\(lo,hi) -> abs(hi-lo)) <$$> sigStat (minF `both` maxF) vm
     let noGood = contains ((>5)//swings) running
     let spikeg = sortBy ( comparing (fst)) $ minInterval 0.1 $ notDuring exclude $ notDuring noGood spike
     return $ Just spikeg
  print $ (sess, length spks)

