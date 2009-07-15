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


type QState = (Session)

getSession = id `fmap` get

evInDuration (t,_) (t1,t2, _) = t<t2 && t>t1

answers = return
 
double :: Double
double = undefined

signals :: Reify a => String -> a-> StateT QState IO [Signal a]
signals nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/signals/"++nm++"/"++fn 
          answers . catMaybes $ map reify sigs) 
      (liftIO (print $ "dir not found:" ++nm) >> answers [])

signal :: Reify a => String -> a-> ListT (StateT QState IO) (Signal a)
signal nm a = ListT $ signals nm a

events :: Reify (Event a) => String -> a-> StateT QState IO [Event a]
events nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/events/"++nm++"/"++fn
          answers . catMaybes $ map reify $ concat utevs) 
      (liftIO (print $ "dir not found:" ++nm) >> answers [])

event :: Reify a => String -> a-> ListT (StateT QState IO) (Event a)
event nm a = ListT $ events nm a

durations :: Reify (Duration a) => String -> a-> StateT QState IO [Duration a]
durations nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/durations/"++nm++"/"++fn
          answers . catMaybes $ map reify $ concat eps)
      (liftIO (print $ "dir not found:" ++nm) >> answers [])            

duration :: Reify a => String -> a-> ListT (StateT QState IO) (Duration a)
duration nm a = ListT $ durations nm a


inLastSession :: StateT QState IO a -> IO a
inLastSession sma = do
  s <- lastSession "/home/tomn/sessions/"
  fst `fmap`  runStateT sma s

inSession :: Session -> StateT QState IO a -> IO a
inSession s sma =  fst `fmap`  runStateT sma s


inNewSession :: StateT QState IO a -> IO a
inNewSession sma = do sess <- newSession "/home/tomn/sessions/"
                      inSession sess sma

inTemporarySession sma = do sess <- newSession "/home/tomn/sessions/"
                            inSession sess sma
                            deleteSession sess

sessionTmax  ::  StateT QState IO Double
sessionTmax  = do
  tstop <- events "tStop" double
  case tstop of
    [] -> return 0
    evs -> return . fst $ maximumBy (comparing fst) evs

--plot :: AskM V ->  StateT QState IO ()
--plot (AskM lm) = do anss <- runListT lm
--                    liftIO $ mapM_ plotWithR anss


--summary :: AskM V ->  StateT QState IO ()
--summary (AskM lm) = do anss <- runListT lm
--                       liftIO $ mapM_ (putStrLn . ppVal) anss

tst1 = do
  saveBinary "/tmp/bugtest" $ ListV [PairV (PairV (cdbl 0) (cdbl 5)) Unit]
  v <- loadBinary "/tmp/bugtest"
  print $ ppVal v


freqDuring :: [Event b] -> [Duration a] -> [Duration (a, Double)]
freqDuring evs durs = map (freqDuring' evs) durs
    where freqDuring' evs dur@(t1, t2, durtag) = 
              (t1, t2, (durtag, 
                        (realToFrac . length $ evs `during` [dur])/(t2-t1)))

during :: [Event a] -> [Duration b] -> [Event a]
during evs durs = concatMap (during' evs) durs
    where during' evs dur = filter (`evInDuration` dur) evs

sigDur :: Signal a -> Duration ()
sigDur (Signal t1 t2 _ _) = (t1,t2, ())

around :: [Signal a] -> [Event b] -> [Signal a]
around sigs evs = catMaybes $ map (around' sigs) evs
    where around' sigs ev@(t,_) = 
              case filter ((ev `evInDuration`) . sigDur) sigs of
                (sig:_) -> Just $ shift (-t) sig
                [] -> Nothing

inout :: [Event a] -> [Event b] -> [Duration (a,b)]
inout [] _ = []
inout _ [] = []
inout ((t1,v1):ins) outs = 
    case dropWhile ((<t1) . fst) outs of
      ((t2,v2):outs') -> (t1,t2,(v1,v2)):inout ins outs'
      [] -> []

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