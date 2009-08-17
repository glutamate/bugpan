{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts#-} 

module Query where

import EvalM
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
import qualified Control.Monad.List as L
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Random

--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L
--import qualified Data.ByteString as BS
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
import Parse
import TNUtils 
import PrettyPrint
import ValueIO
import qualified Data.Binary as B

type QState = (Session)

getSession = id `fmap` get

answers = return
 
double :: Double
double = undefined


--change these to loadUntyped
signals :: Reify a => String -> a-> StateT QState IO [Signal a]
signals nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/signals/"++nm++"/"++fn 
          answers . catMaybes $ map reify $ concat sigs) 
      (liftIO (print $ "dir not found:" ++nm) >> answers [])

signalsDirect :: String ->  StateT QState IO [Signal Double]
signalsDirect nm = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM  fnms $ \fn-> liftIO $ loadSignalsU $ bdir++"/signals/"++nm++"/"++fn 
          return $ concat sigs) 
      (liftIO (print $ "dir not found:" ++nm) >> answers [])

signal :: Reify a => String -> a-> L.ListT (StateT QState IO) (Signal a)
signal nm a = L.ListT $ signals nm a

events :: Reify (Event a) => String -> a-> StateT QState IO [Event a]
events nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/events/"++nm++"/"++fn
          answers . catMaybes $ map reify $ concat utevs) 
      (liftIO (print $ "dir not found:" ++nm) >> answers [])

event :: Reify a => String -> a-> L.ListT (StateT QState IO) (Event a)
event nm a = L.ListT $ events nm a

durations :: Reify (Duration a) => String -> a-> StateT QState IO [Duration a]
durations nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          answers . catMaybes $ map reify $ concat eps)
      (liftIO (print $ "dir not found:" ++nm) >> answers [])            

duration :: Reify a => String -> a-> L.ListT (StateT QState IO) (Duration a)
duration nm a = L.ListT $ durations nm a


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

inSessionNamed :: String -> StateT QState IO a -> IO a
inSessionNamed nm sma = do sess <- loadExactSession $ "/home/tomn/sessions/"++nm
                           inSession sess sma


inApproxSession :: String -> StateT QState IO a -> IO a
inApproxSession nm sma = do sess <- loadApproxSession "/home/tomn/sessions/" nm
                            inSession sess sma



sessionTmax  ::  StateT QState IO RealNum
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