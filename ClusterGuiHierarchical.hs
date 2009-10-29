{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

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
import Numeric.LinearAlgebra hiding (flatten)
import Foreign.Storable
import Math.Probably.KMeans 
import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.IORef
import Database
import NewSignal
import Graphics.UI.Gtk hiding (Signal)
import SpikeDetect

main = spikeDetectIO

autoSpikes sigNm = do
  sigs <- take 1 `fmap` signalsDirect sigNm 
  threshD@((_,thresh):_) <- durations "threshold" (1::Double)
  io $ initGUI
  Session bdir _ <- getSession
  let sessNm = last $ splitBy '/' bdir
  window <- io $ windowNew
  buttonGo <- io $ buttonNew
  lIm <- io $ imageNew
  rIm <- io $ imageNew
  indivIms <- forM [0..11] $ \i-> io $ imageNew
  nclustsRef <- io $ newIORef 4
  hbox1 <- io $ hBoxNew True 10
  hbox2 <- io $ hBoxNew True 10
  --hbox3 <- io $ hBoxNew True 10
  vbox <- io $ vBoxNew False 10
  io $ boxPackStart hbox1 lIm PackGrow 0    
  io $ boxPackStart hbox1 rIm PackGrow 0    
  {-cbs <- io $ forM [0..11] $ \i -> do 
                     cb <- checkButtonNewWithLabel (show i)
                     io $ boxPackStart hbox2 cb PackGrow 0                    
                     return cb
  io $ forM_ [0..3] $ \h-> do
                     hb <- vBoxNew True 5
                     forM [0..2] $ \v-> do
                              vb <- hBoxNew True 5
                              -- but <- buttonNew
                              --print (v, h, (v*4+h))
                              boxPackStart vb (indivIms!!(v*4+h)) PackGrow 0
                              boxPackStart hb vb PackGrow 0
                     boxPackStart hbox3 hb PackGrow 0
     
                              -}
                          

  --let sd = stdDevF `sigStat` sigs
  let putatives = if thresh <0  
                        then crossesDown threshD sigs 
                        else crossesUp threshD sigs
  let waveforms =  limitSigs' (-0.001) 0.001 $ around putatives $ sigs
  --ask $ plot $sigs :+: threshD
  io $ print $ length putatives
  let alignWaveforms = --limitSigs' (-0.0006) 0.0006 $ 
                         unjitter $ 
                         alignBy (centreOfMass . ((square . square) <$$>) . limitSigs (-0.0005) 0.0005 ) $ 
                         waveforms 
  let alignedEvents = shiftEach (map (negate . (+0.001). fst) $ sigStarts alignWaveforms) putatives
  let realignedWaveforms = limitSigs' (-0.0005) 0.0005 $ around alignedEvents sigs

  --io $ mapM print $ map length $ map sigToList $ realignedWaveforms
  let dataMatrix = fromLists $ map sigToList $ realignedWaveforms

  --let sigu = upsample 5 sigs
 --PCA
  --let nclust = 4
  let neigenVecs = 3
  let datMatSubMeans = fromColumns $ map vecSubMean $ toColumns dataMatrix
  let covDM = cov datMatSubMeans
  let (eigvals, eigvecs) = eig covDM
  let evs = take neigenVecs $ map snd $ reverse $ sortBy (comparing fst) $ zip (fmap realPart $ toList eigvals) [0..]
  let featureVector = fromColumns $ map (mapVector realPart . ((toColumns eigvecs)!!)) evs
  let finalData = trans featureVector <> trans datMatSubMeans
  let pts =  {- map toList $ -} toColumns $ finalData
  let nobs = realToFrac $ length $ alignWaveforms
  io $ print $ length pts
  tree <- io $ newIORef $ hierarchicalCluster (meanDists $ euclidianOn snd) $ zip [0..] pts

{-  let likeBic nclusters = 
          let clustered = kmeansOn (snd) nclusters $  zip [0..] pts
              --clusteredvs = map (map (listToPoint . snd)) clustered
              gaussians = map (fitMultiGauss . map fromList) $ map (map snd) clustered
              weights = map ((/nobs) . realToFrac . length . map snd) clustered
              wgausses = zip weights gaussians
              likelihood = sum $map (wgausses `multiGaussMixture`) $ map (fromList) pts
              nparams = (realToFrac $ nclusters*neigenVecs*(neigenVecs*(neigenVecs-1)))/2 --means, covariance matrix          
              bic = -2*likelihood +nparams*log nobs
          in (clustered, undefined) {-if (all (not . isNaN . det . snd) gaussians) 
                 then Just (clustered, (likelihood, bic))
                 else Nothing -}
-}
  let displayData nclusters =
          {-let clustered = fst $ likeBic nclusters
              clusteredvs = map (map (listToPoint . snd)) clustered
              idxs = map (map fst) clustered
              sigsav = map (take 1 . averageSigs . (alignWaveforms `indexMany`)) idxs
              evss = map (minInterval 0.001 . (putatives `indexMany`)) idxs
              realnclust = length clusteredvs -}
          do                     
                     io $ putStr "pca..."
                     io $ putStrLn $ " done for "++show (length pts)++" events"
                     io $ putStrLn "clustering..."
                     --io $ writeIORef nclustsRef realnclust
                     io $ putStr "making sig avgs... "         
                     plotSize 500 500 
                     (Node l r) <- io $ readIORef tree
                     let lints = map fst $ flatten l
                     lpic <- askPics $ plot $ realignedWaveforms `indexMany` lints
                     let rints = map fst $ flatten r
                     rpic <- askPics $ plot $ realignedWaveforms `indexMany` rints
                     io $ putStr "done\nmaking clusters... "
                     io $ print lints
                     io $ print rints
                     io $ imageSetFromFile (lIm) $ head lpic
                     io $ imageSetFromFile (rIm) $ head rpic
                     {-cluspic <- askPics $ plot $ LabelConsecutively $ map (clusteredvs!!) [0..realnclust-1]
                     plotSize 300 200
                     forM_ (zip [0..] $ map (alignWaveforms `indexMany`) idxs) $ 
                               \(i,sigs)-> do
                                             pic <- askPics $ plot sigs
                                             io $ imageSetFromFile (indivIms!!i) $ head pic
                     io $ putStrLn "done"
                     --fail "foo"
                     io $ imageSetFromFile avsIm $ head avspic
                     io $ imageSetFromFile clusIm $ head cluspic
                     forM_ [0..realnclust-1] $ \i -> io $ widgetShow (cbs!!i)
                     forM_ [realnclust..11] $ \i -> io $ widgetHide (cbs!!i)
                     forM_ [realnclust..11] $ \i -> io $ imageClear (indivIms!!i) --hide images
                     let saveAction = do
                             checked <- forM [0..11] $ \i -> ifM (toggleButtonGetActive (cbs!!i))
                                                                 (return $ Just i)
                                                                 (return Nothing)
                             print $ catMaybes checked
                             let finalEvs = concatMap (evss!!) $ catMaybes checked
                             inApproxSession sessNm $ do
                                        storeAsOvwrt "spikes" finalEvs
                             putStrLn $ "saved "++show (length finalEvs)++" spikes..."
                             --print finalEvs
                             return ()
                     io $ onClicked buttonGo saveAction -- (putStrLn "Hello World")
-}

                     return ()
                     {-let imgs = map imgToHtml $ (avspic++cluspic)
                     let ch n = n +++ checkbox ("ch"++n) ("ch"++n)
                     res <- undefined {-jsToStrAssoc `fmap` (lift $ askDeskWeb $ form![method "post"] << 
                                                       (imgs+++
                                                       paragraph noHtml+++ 
                                                       (map (ch . show) [0..nclusters-1])+++
                                                       "store as event"+++ 
                                                       textfield "evname"+++ 
                                                       " or change number of clusters " +++
                                                       textfield "numclusters" +++
                                                       submit "storebtn" "Store")) -}
                     io $print res
                     case lookup "numclusters" res of
                       Just n -> displayData $ read n
                       Nothing -> return (res, evss, nclusters)
                 -}
  io $ set window [ containerBorderWidth := 10,
                    containerChild := vbox ]
  io $ boxPackStart vbox hbox1 PackGrow 0
  --io $ boxPackStart vbox hbox3 PackGrow 0
  io $ boxPackStart vbox hbox2 PackGrow 0
--  io $ boxPackStart vbox hbox3 PackGrow 0
  --io $ boxPackStart hbox1 avsIm PackGrow 0
  --io $ boxPackStart hbox1 clusIm PackGrow 0
  --io $ boxPackStart hbox2 buttonGo PackGrow 0
  io $ set buttonGo [ buttonLabel := "Save" ]
  --io $ onClicked buttonGo (putStrLn "Hello World")
  io $ onDestroy window mainQuit
  io $ widgetShowAll window
  displayData 10
  io $ mainGUI
{-
  --calc likelihood
  --calc BIC

  --return (sigsav, evss)
  --io $ print $ length sigsav
  (res, evss, nclust) <- displayData 4
  let isIn = filter ((`elem` (map fst res)) . ("ch"++) . show) [0..nclust-1]
  let finalEvs = concatMap (evss!!) isIn
  let Just nm = lookup "evname" res
  io $ print nm
  --lift $ --askDeskWeb $ thediv << "hello world"
  --ask $ plot $ map listToPoint pts
  --ask $ plot $ clusteredvs!!0 :+: clusteredvs!!1 :+: clusteredvs!!2 -- :+: clusteredvs!!3
  --ask $ plot $ take 1000 $ alignWaveforms
  --io $ print $ (rows finalData, cols finalData)

  --covariance matrix

-}

  --ask $ plot $ take 100 $ alignWaveforms 
  --io $ print $ dataMatrix
  return ()

spikeDetectIO = do 
  (snm:_) <-getArgs 
  inApproxSession snm $ do 
                  --openReplies
                  --initUserInput
                  --plotSize 490 329
                  --overDur <- unitDurations overDurNm
                  autoSpikes "normV"
                  return ()
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
                                           

