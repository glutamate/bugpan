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
import Graphics.UI.Gtk.Gdk.Events

main = spikeDetectIO

autoSpikes sigNm = do
  sigs <- signalsDirect sigNm 
  running <- durations "running" ()
  io $ initGUI
  let nsigs = length sigs

  Session bdir _ <- getSession
  let sessNm = last $ splitBy '/' bdir
  window <- io $ windowNew
  buttonNext <- io $ buttonNew
  buttonUp <- io $ buttonNewWithMnemonic "_kUp"
  buttonDown <- io $ buttonNewWithMnemonic "_jDown"
  indivIms <- forM [0..15] $ \i-> io $ imageNew
  nclustsRef <- io $ newIORef 4
  hbox1 <- io $ hBoxNew True 10
  hbox2 <- io $ hBoxNew True 10
  --hbox3 <- io $ hBoxNew True 10
  vbox <- io $ vBoxNew False 10
  entry <- io $ entryNew

  labelProg <- io $ labelNew $ Nothing

  currentThreshold <- io $ newIORef 0
  currentStart <- io $ newIORef 0
  currentNumberPerView <- io $ newIORef 8

  let updateProg = do st <- readIORef currentStart
                      num <- readIORef currentNumberPerView
                      labelSetText labelProg  $ show st++"-"++show (st+num) ++"/"++show nsigs
  io $ updateProg

  io $ forM_ [0..3] $ \h-> do
                     hb <- vBoxNew True 5
                     forM [0..3] $ \v-> do
                              vb <- hBoxNew True 5
                              -- but <- buttonNew
                              --print (v, h, (v*4+h))
                              boxPackStart vb (indivIms!!(v*4+h)) PackGrow 0
                              boxPackStart hb vb PackGrow 0
                     boxPackStart hbox2 hb PackGrow 0

  let displayData  = do
                     io $ putStr "pca..."
                     -- plotSize 500 500 
                     -- luspic <- askPics $ plot $ LabelConsecutively $ map (clusteredvs!!) [0..realnclust-1]
                     plotSize 350 220

                     start <- io $ readIORef currentStart
                     num <- io $ readIORef currentNumberPerView
                     let cursigs = take num $ drop start sigs
                     tv <- io $ readIORef currentThreshold
                     pics <- askPics $ plotManyBy (map sigDur cursigs) $ DownTo 500 cursigs :+: tv `tag` map sigDur cursigs
                     forM_ (zip [0..(num-1)] cursigs) $ \(i,sig)-> do
                       --io $ print (i, num, length cursigs)
                       --pic <- askPics $ plot $ DownTo 500 [cursigs!!i] :+: tv `tag` [sigDur $ cursigs!!i]
                       io $ imageSetFromFile (indivIms!!i) $ (pics!!i)
                       return ()

                     io $ putStrLn "done"
                     --fail "foo"                    
                     return ()
               
  let saveAction = do
        start <- io $ readIORef currentStart
        num <- io $ readIORef currentNumberPerView
        let cursigs = take num $ drop start sigs
        threshval <- io $ readIORef currentThreshold
                           
        let spikes = if threshval >0
                       then crossesUp (threshval `tag` (map sigDur cursigs)) cursigs
                       else crossesDown (threshval `tag` (map sigDur cursigs)) cursigs

        writeIORef currentStart (start+num)
        io $ updateProg
        inApproxSession sessNm $ do
                     storeAsAppend "spikesManual" spikes
                     displayData
                                --putStrLn $ "saved "++show (length finalEvs)++" spikes..."
                                --print finalEvs

                                
        return ()

  let dispatchKey "j" = do io $ modifyIORef currentThreshold (\x->x-1)
                           displayData
      dispatchKey "k" = do io $ modifyIORef currentThreshold (\x->x+1)
                           displayData
      dispatchKey "space" = do io $ saveAction 
      dispatchKey "BackSpace" = do curnum<- io $ readIORef currentNumberPerView 
                                   io $ widgetHide $ indivIms!!(curnum-1)
                                   io $ writeIORef currentNumberPerView (curnum-1)
                                   displayData
      dispatchKey "Insert"    = do curnum<- io $ readIORef currentNumberPerView 
                                   io $ widgetShow $ indivIms!!(curnum)
                                   io $ writeIORef currentNumberPerView (curnum+1)
                                   displayData

      dispatchKey s = do io $ putStrLn $ "hit unknown key: "++show s

  io $ onClicked buttonNext saveAction -- (putStrLn "Hello World")
  io $ onClicked buttonUp $ do
                     io $ modifyIORef currentThreshold (+1)
                     inApproxSession sessNm $ do
                              displayData
  io $ onClicked buttonDown $ do
                     io $ modifyIORef currentThreshold (\x->x-1)
                     inApproxSession sessNm $ do
                              displayData

 
  io $ onEntryActivate entry $ do txt <- entryGetText entry
                                  case safeRead txt of
                                    Nothing -> return ()
                                    Just x ->  do writeIORef currentThreshold x
                                                  inApproxSession sessNm $ do
                                                      displayData
  io $ widgetAddEvents window [KeyPressMask]
  --io $ vbox `on` keyPressEvent $ print "hello"
  io $ onKeyPress window $ \evt -> case evt of
                                     Key { eventKeyName = s } -> do
                                              inApproxSession sessNm (dispatchKey s)
                                              return False
                                     _ -> return False
  io $ set window [ containerBorderWidth := 10,
                    containerChild := vbox ]
  io $ boxPackStart vbox hbox1 PackGrow 0
  --io $ boxPackStart vbox hbox3 PackGrow 0
  io $ boxPackStart vbox hbox2 PackGrow 0
--  io $ boxPackStart vbox hbox3 PackGrow 0
  io $ boxPackStart hbox1 labelProg PackGrow 0
  --io $ boxPackStart hbox1 buttonNext PackGrow 0
  --io $ boxPackStart hbox1 buttonUp PackGrow 0
  --io $ boxPackStart hbox1 buttonDown PackGrow 0
  --io $ boxPackStart hbox1 clusIm PackGrow 0
  --io $ boxPackStart hbox2 buttonGo PackGrow 0
  io $ set buttonNext [ buttonLabel := "Next" ]
  --io $ onClicked buttonGo (putStrLn "Hello World")
  io $ onDestroy window mainQuit
  io $ widgetShowAll window
  displayData 
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
                                           

