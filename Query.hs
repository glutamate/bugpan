{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSignatures, FlexibleContexts#-} 

module Query where

import EvalM hiding (ListT)
import Eval
import Expr
import Data.Maybe
import Data.List
import Numbers
{-import ImpInterpret
import Compiler 
import Stages
import Traverse
import Transform-}
import Control.Monad
import Control.Monad.List
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Random
import System.Cmd
--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L
--import Data.ByteString.Internal
import qualified Data.Binary as B
import Numeric
import Traverse
import Transform
--import Stages
import Data.Ord
--import Charts
import Control.Concurrent
import Database
import HaskSyntaxUntyped
import QueryTypes


evInEpoch ev ep = let (t1, t2) = epTs ep 
                      tev = evTime ev
                  in tev<t2 && tev>t1

answers = return
 
signals :: String -> StateT Session IO [Signal]
signals nm = do
  Session bdir t0 <- get
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/signals/"++nm++"/"++fn 
          answers sigs) 
      (liftIO (print "dir not found") >> answers [])

events :: String -> StateT Session IO [Event]
events nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/events/"++nm++"/"++fn
          answers  $ map vToEvent $ concat utevs) 
      (liftIO (print "dir not found") >> answers [])

durations ::  String -> StateT Session IO [Duration]
durations nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/epochs/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/epochs/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/epochs/"++nm++"/"++fn
          answers $ map vToDuration $ concat eps)
      (liftIO (print "dir not found") >> answers [])            

inLastSession :: StateT Session IO a -> IO a
inLastSession sma = do
  s <- lastSession "/home/tomn/sessions/"
  fst `fmap`  runStateT sma s

inSession :: Session -> StateT Session IO a -> IO a
inSession s sma =  fst `fmap`  runStateT sma s


--plot :: AskM V ->  StateT Session IO ()
--plot (AskM lm) = do anss <- runListT lm
--                    liftIO $ mapM_ plotWithR anss


--summary :: AskM V ->  StateT Session IO ()
--summary (AskM lm) = do anss <- runListT lm
--                       liftIO $ mapM_ (putStrLn . ppVal) anss

tst1 = do
  saveBinary "/tmp/bugtest" $ ListV [PairV (PairV (cdbl 0) (cdbl 5)) Unit]
  v <- loadBinary "/tmp/bugtest"
  print $ ppVal v


testQ1 = inLastSession $ do
           spike <- events "spike"
           stim  <- durations "inputRate"
           return ()
--           plot (stim, spike `freqDuring` stim)
 --          plot (zip stim $ spike `freqDuring` stim)
--           plot . signals $ "vm" 

freqDuring :: [Event] -> [Duration] -> [Duration]
freqDuring evs eps = map (freqDuring' evs) eps
    where freqDuring' evs (t1, t2, vd) = 
              (t1, t2, cdbl ((realToFrac .length $ filter (\(t,vev)-> t > t1 && t < t2 ) evs)/(t2-t1)))
             
plotSig :: (MonadIO m) => V -> m ()
plotSig = liftIO . plotWithR
        
plotWithR :: V -> IO ()
plotWithR (SigV t1 t2 dt sf) = do
  (r::Int) <- randomRIO (0,10000)
  let datfile= "/tmp/bugplot"++show r
  let rfile = "/tmp/bugplot"++show r++".r"
  writeFile datfile $ unlines $ map (\t->show . unsafeVToDbl $ sf t) [0..round $ (t2-t1)/dt]
  writeFile rfile $ concat [
                 "x11(width=10,height=7)\n",
                 "dat <- ts(scan(\"", datfile, "\"), start=", show t1, ", frequency=", show (1/dt),")\n", 
                 "plot(dat)\n", 
                 "z<-locator(1)\n",
                 "q()"]
  system $ "R --vanilla --slave < "++rfile
  removeFile datfile
  removeFile rfile
  return ()

{-plot :: [V] -> IO ()
plot vs = do --let g = map ansToPlot ans
             plotGraph (valsToGraph vs)
             return ()
          

valsToGraph :: [V] -> Graph
valsToGraph vs = foldl1 (<+>) $ map vToPlot vs
    where vToPlot (SigV t1 t2 dt sf)= toGraph ((toPlot $map (\t -> (t, unsafeVToDbl $ sf t)) [t1, t1+dt..t2])%Lines)
          vToPlot e  | isEvent e = toGraph ((toPlot [(evTime e, unsafeVToDbl $ evTag e)])%FilledCircles)
          vToPlot ep | isEpoch ep = 
                         let (t1,t2) = epTs ep 
                             epvl = unsafeVToDbl $ epTag ep in
                         toGraph ((toPlot [(t1, epvl), (t2, epvl)])%Lines) 

-}

{-testQ = do s <- lastSession "/home/tomn/sessions/"
           print s
           qres <- runAskM s $ signals "vm"
           mapM plotWithR qres
                         
           --mapM print qres
           return () -}