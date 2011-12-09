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



main = do 
  sessApprox:dowhat:rest <- getArgs
  let remove_fail "fail_resolve" = sessApprox
      remove_fail s = s
  sess <- fmap remove_fail $ resolveApproxSession sessionsRootDir sessApprox

  createDirectoryIfMissing False $ take 6 sess

  when ('1' `elem` dowhat) $ epspSigs sess
  when ('2' `elem` dowhat) $ measNoise sess
  when ('3' `elem` dowhat) $ measAmps sess
  when ('4' `elem` dowhat) $ measNPQ sess
  when ('5' `elem` dowhat) $ summary sess
  when ('6' `elem` dowhat) $ simulate sess rest
  return ()

simulate sess rest = runRIO $ do
  let n = read $ head rest
  sigs <- fmap ( map stagger . zip [1..] ) $ sample $ fakesam $ n
  io $ encodeFile (take 6 sess++"/sigs_"++take 6 sess++"_epsps") $ LoadSignals sigs 
  io $ writeFile (take 6 sess++"/noisePars") $ show (log thetaHat, sigmaHat, log obsHat)


summary sess = do
  changeWorkingDirectory $ take 6 sess
  h <- openFile (take 6 sess++".tex") WriteMode 
  let puts = hPutStrLn h
      plotIt nm obj = do gnuplotToPS (nm++".eps") $ obj
                         system $ "epstopdf "++nm++".eps"
                         puts $"\\includegraphics[width=16cm]{"++nm++"}"
  meass <- fmap catMaybes $ inEverySession $ whenContinues sess $ do
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
     let noiseSigs = take 50 $ limitSigs' (-0.11) (-0.01) $ around (spikeg) $ vm
     let epspSigs = during (durAroundEvent (0.03) 0.07 spikeg) vm 
     let aroundSpike = baseline (-0.003) 0.003 $ limitSigs' (-0.05) 0.05 $ around (spikeg) $ vm
     let ampPeak = snd $ head $ peak $ take 1 $ QU.averageSigs $ take 100 $ aroundSpike
     let tpeak = fst $ head $ peak $ take 1 $ QU.averageSigs $ aroundSpike
     let measDur  = measureBl (-0.003, 0.003) (tpeak-0.015,0.015+tpeak) vm spikeg
     return $ Just measDur
  puts $ unlines     ["\\documentclass[11pt]{article}",
     "%include lhs2TeX.fmt",
     "%include polycode.fmt",
     "\\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}",
     "\\usepackage{graphicx}",
     "\\begin{document}",
     sess,
     "\n\ntraditionally measured EPSP amplitudes\n\n"]

  plotIt ("epsps_"++ take 6 sess) $ Points [PointSize 1] $ concat meass

  nms <- fmap read $ readFile ("sessions")
  sigs <- fmap concat $ forM nms $ \sessNm-> do 
            LoadSignals sigs <-  decodeFile $ "sigs_"++take 6 sessNm++"_epsps" 
            return sigs

  let wf@(Signal _ _ sv) = baselineSig 0.003 $ averageSigs $ sigs
  let wfAmp = foldl1' max $ L.toList sv
  puts $ "wfamp= "++show wfAmp++"\n"
  plotIt "wf" wf
  let ffile = (unzip3 .  sortBy (comparing fst3) . map read . lines)
  (t0s'::[Double], amps::[Double],sds::[Double]) <- fmap ffile  $ readFile ("epsps")
  plotIt ("epspsou_"++ take 6 sess) $ Points [PointSize 1] $ zip t0s' $ map (*wfAmp) amps

  puts "\\end{document}"
  hClose h

  system $ "pdflatex "++take 6 sess++".tex"
  return ()

epspSigs sess = do 
  nms <- fmap catMaybes $ inEverySession $ whenContinues sess $ do
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
     let noiseSigs = take 50 $ limitSigs' (-0.11) (-0.01) $ around (spikeg) $ vm
     let epspSigs = during (durAroundEvent (0.03) 0.07 spikeg) vm 
     let aroundSpike = baseline (-0.003) 0.003 $ limitSigs' (-0.05) 0.05 $ around (spikeg) $ vm
     let ampPeak = snd $ head $ peak $ take 1 $ QU.averageSigs $ take 100 $ aroundSpike
     let tpeak = fst $ head $ peak $ take 1 $ QU.averageSigs $ aroundSpike
     let measDur  = measureBl (-0.003, 0.003) (tpeak-0.015,0.015+tpeak) vm spikeg
     ask $ SaveSignals (take 6 sess++"/sigs_"++ take 6 sessionIdentifier ++ "_noise") noiseSigs
     ask $ SaveSignals (take 6 sess++"/sigs_"++ take 6 sessionIdentifier ++ "_epsps") epspSigs
     liftIO $ gnuplotToPNG (take 6 sess++"/epsps_"++ take 6 sessionIdentifier ++ ".png") $ measDur
     return $ Just sessionIdentifier
  writeFile (take 6 sess++"/sessions") $ show nms

measNoise sess = runRIO $ do 
  LoadSignals sigs <- io $ decodeFile $ take 6 sess++"/sigs_"++take 6 sess++"_noise"
  let initialV = L.fromList [-2, 2::Double, -5, -60]
  io$ print $ initialV
  io$ print $ tmax/dt
  iniampar <- sample $ initialAdaMet 100 5e-3 (posteriorNoiseV sigs) initialV
  froampar <- runAndDiscard 500 (show . ampPar) iniampar $ adaMet False (posteriorNoiseV sigs)
  vsamples<- runAdaMetRIO 200 True froampar (posteriorNoiseV sigs)
  let [logtheta, sigma, logobs, _] = L.toList$   runStat meanF vsamples
  io $ writeFile (take 6 sess++"/noisePars") $ show (logtheta, sigma, logobs)
  return ()

measAmps sess = runRIO $ do
  (logtheta, sigma, logobs) <- fmap read $ io $ readFile (take 6 sess++"/noisePars")
  let covM = fillM (np,np) $
              \(i,j)-> ((covOU (exp logtheta) (sigma::Double)) (toD i)) (toD j)+ifObs i j (exp logobs)
  let invDetails = invlndet covM
  nms <- fmap read $ io $ readFile (take 6 sess++"/sessions")
  sigs <- fmap concat $ forM nms $ \sessNm-> do 
            LoadSignals sigs <- io $ decodeFile $ take 6 sess++"/sigs_"++take 6 sessNm++"_epsps" 
            return sigs
  let wf = baselineSig 0.003 $ averageSigs $ sigs
  h<- io $ openFile (take 6 sess++"/epsps") WriteMode 
  forM_ sigs $ \sig@(Signal dt t0 _) -> do
      let initialV = L.fromList [-60,1]
      iniampar <- sample $ initialAdaMet 100 5e-3 (posteriorSigV wf invDetails sig) initialV
      froampar <- runAndDiscard 2000 (show . ampPar) iniampar $ adaMet False (posteriorSigV wf invDetails sig)
      vsamples<- runAdaMetRIO 3000 True froampar (posteriorSigV wf invDetails sig)
      let (amp,sd) = both meanF stdDevF `runStat` map (@>1) vsamples
      io $ print (t0,amp,sd)
      io $ hPutStrLn h $ show (t0, amp,sd)
      --plotPts $ zip [0..] $ map (@>1) vsamples
      return ()
  io $ hClose h
  return ()
 
measNPQ sess = runRIO $ do
  let ffile = (unzip3 .  sortBy (comparing fst3) . map read . lines)
  (t0s'::[Double], amps,sds) <- io $ fmap ffile  $ readFile (take 6 sess++"/epsps")
  let tsamps = zip t0s' amps
      --tsamps = filter ((<3) . (\(t, amp)-> zscore tsamps' (t,amp))) tsamps'
      t0s = map fst tsamps
  let weighCurve' = map (weighRegression tsamps ) t0s
      maxPcurve = foldl1 max weighCurve'
      pcurve = map (/(maxPcurve)) weighCurve'
  let globalSd = sqrt $ runStat (before meanF (**2)) sds

  let initialV = L.fromList [100,log 0.15,0.8,log 0.02]

  let nsam = 40000
      nfrozen = 20000 
  io $ print $ posteriorNPQV amps pcurve globalSd initialV
  iniampar <- sample $ initialAdaMet 500 5e-3 (posteriorNPQV amps pcurve globalSd) initialV
  io $ putStr "inipar ="
  io $ print $ ampPar iniampar
  froampar <- runAndDiscard nsam (showNPQV . ampPar) iniampar $ adaMet False (posteriorNPQV amps pcurve globalSd)
  io $ putStr "frozenpar ="
  io $ print $ ampPar froampar
  vsamples <- runAdaMetRIO nfrozen True froampar (posteriorNPQV amps pcurve globalSd) 
  io $ writeFile (take 6 sess++"/npq_samples") $ show vsamples
  let (mean,sd) =  (both meanF stdDevF) `runStat` vsamples 
  io $ putStrLn $ intercalate "\t" $ map (minWidth 8) $ words "n cv phi q"
  io $ putStrLn $ showNPQV mean
  io $ putStrLn $ showNPQV sd 
  return ()

fst3 (x,_,_) = x

whenContinues sess mma = do
      conts <- durations "continues" "foo"
      sessid <- getSessionName
      case conts of
        [] -> if (sessid==sess) then mma else return Nothing
        (_,s):_ | s `isPrefixOf` sess -> mma
                | otherwise -> return Nothing
  