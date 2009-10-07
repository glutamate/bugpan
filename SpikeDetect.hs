{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

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
import EvalM
import Numeric.LinearAlgebra
import Foreign.Storable
import Math.Probably.KMeans 

main = spikeDetectIO

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

--sinSig :: Signal Double
--sinSig = listToSig 0.1 0 $ map sin $ [0, 0.4..4]

mean x = sumColumns x / fromIntegral (rows x)
    where sumColumns m = constant 1 (rows m) <> m


cov x = (trans xc <> xc) / fromIntegral (rows x -1)
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m

zipWithMap :: (a->b) -> [a] -> [(a,b)]
zipWithMap f xs = map (\x->(x, f x)) xs

  {-let (m1::Matrix Double) = (2><2) [ 1, 1,
                                     -2,4]
  io $ print $ eig m1-}

vecSubMean vec = let lst = toList vec
                     mean = runStat meanF lst
                     submean x = x-mean
                 in mapVector submean vec
                 

--runStatVec :: (Storable b) => Fold b c -> Vector b -> c
--runStatVec (F f x c _) v = c . (foldVector f x)

listToPoint (x:y:_) = (x,y)

indexMany :: [a] -> [Int] -> [a]
indexMany = map . (!!) 

autoSpikes sigNm = do
  sigs <- take 10 `fmap` signalsDirect sigNm 
  let sd = stdDevF `sigStat` sigs
  --let sigu = upsample 5 sigs
  let putatives = crossesDown (((*5) . negate) <$$> sd) sigs `catevents` 
                  crossesUp ((*5) <$$> sd) sigs
  let waveforms = limitSigs' (-0.001) 0.001 $ around putatives $ sigs
  let alignWaveforms = take 1000 $ downsample 10 $ unjitter $ upsample 10 $ 
                       alignBy (centreOfMass . (square <$$>) . (limitSigs (-0.0005) 0.0005)) $ 
                       waveforms 
  --PCA
  let dataMatrix = fromLists $ map sigToList $ alignWaveforms
  let datMatSubMeans = fromColumns $ map vecSubMean $ toColumns dataMatrix
  let covDM = cov datMatSubMeans
  let (eigvals, eigvecs) = eig covDM
  let evs = take 3 $ map snd $ reverse $ sortBy (comparing fst) $ zip (fmap realPart $ toList eigvals) [0..]
  let featureVector = fromColumns $ map (mapVector realPart . ((toColumns eigvecs)!!)) evs
  let finalData = trans featureVector <> trans datMatSubMeans
  let pts =  map toList $ toColumns $ finalData
  let clustered = kmeansOn (snd) 3 $  zip [0..] pts
  let clusteredvs = map (map (listToPoint . snd)) clustered
  let idxs = map (map fst) clustered
  let sigsav = map (head . averageSigs . (alignWaveforms `indexMany`) . map round) idxs 
  io $ print $ length sigsav
  ask $ plot $ sigsav
  --ask $ plot $ map listToPoint pts
  ask $ plot $ clusteredvs!!0 :+: clusteredvs!!1 :+: clusteredvs!!2 -- :+: clusteredvs!!3
  ask $ plot $ take 1000 $ alignWaveforms
  --io $ print $ (rows finalData, cols finalData)

  --covariance matrix


  --ask $ plot $ take 100 $ alignWaveforms 
  --io $ print $ dataMatrix
  return ()


spikeDetectIO = do 
  (snm:overDurNm:_) <-getArgs
  inApproxSession snm $ do
                  openReplies
                  initUserInput
                  plotSize 490 329
                  overDur <- unitDurations overDurNm
                  autoSpikes "normV"
                  --normV <- signalsDirect "normV"
                  --spikeDetect [overDur] normV []


spikeDetect overs normV spks = do
  let over = head overs
  io $ putStrLn $ "currently considering "++show (length over)++" durations, "++show (length spks)++" spikes"
  userChoice [('s', "show normV and existing spikes", 
                  do --normV <- signalsDirect "normV"
                      spikes <- events "spikes" () 
                      ask $ plotManyBy over $ normV :+: ("stored spikes", (-10) `tagd` spikes) :+: 
                              ("New spikes", (-12) `tagd` spks)
                      spikeDetect overs normV spks),
              ('u', "undo restrict",
                   case overs of 
                     [o] -> spikeDetect overs normV spks
                     o:os->spikeDetect os normV spks),
              ('c', "clear detected spikes",
                    spikeDetect overs normV []),
              ('q', "quit",
                  return ()),
              ('d', "delete stored spikes",
                  do deleteValue "spikes"
                     spikeDetect overs normV spks),
              ('v', "save detected spikes",
                    do storeAsOvwrt "spikes" $ sortBy (comparing fst) spks
                       spikeDetect overs normV spks),
              ('r', "restrict trials", 
                   do ndrop <- userValue "number to drop"
                      ntake <- userValue "number to take"
                      spikeDetect ((take ntake $ drop ndrop over):overs) normV spks),
              ('f' , "fixed threshold", 
                   do thr <- userValue "threshold"
                      let thresh = durd (thr)
                      let spikes = during over $ crossesDown thresh normV
                      ask $ plotManyBy over $ normV :+: thresh :+: thr `tagd` spikes
                      ifM (userConfirm "accept spikes")
                          (spikeDetect overs normV (spks++spikes))
                          (spikeDetect overs normV spks))]  
                                           

