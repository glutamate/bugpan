{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances #-} 

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
--import qualified Control.Monad.List as L
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
          return . catMaybes $ map reify $ concat sigs) 
      (return [])

signalsDirect :: String -> StateT QState IO [Signal Double]
signalsDirect nm = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadSignalsU $ bdir++"/signals/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])


events :: Reify (Event a) => String -> a-> StateT QState IO [Event a]
events nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/events/"++nm++"/"++fn
          return . catMaybes $ map reify $ concat utevs) 
      (return [])

eventsDirect :: LoadDirectly [(Double,a)] => String -> a-> StateT QState IO [Event a]
eventsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/events/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])

durations :: Reify (Duration a) => String -> a-> StateT QState IO [Duration a]
durations nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          return . catMaybes $ map reify $ concat eps)
      (return [])            

durationsDirect :: LoadDirectly [(Double,a)] => String -> a-> StateT QState IO [Event a]
durationsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/durations/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])


storeAs :: Reify a => String -> [a] -> StateT QState IO ()
storeAs nm vls = do 
  let vs = map pack vls
  let t = typeOfReified $ head vls
  let subDir = case t of
                 SignalT _ -> "signals/"
                 PairT (NumT (Just RealT)) _ -> "events/"
                 PairT (PairT (NumT (Just RealT)) (NumT (Just RealT))) _ -> "durations/"
                 _ -> error $ "cannot store "++nm++": unknown type"
  Session bdir t0 <- getSession
  liftIO $ createDirectoryIfMissing False $ oneTrailingSlash bdir++subDir++nm
  liftIO $ saveVs (oneTrailingSlash bdir++subDir++nm++"/stored") vs
  

instance (Reify a, QueryResult [a]) => QueryResult (StoreAs a) where
    qResThroughSession sa@(StoreAs nm val) = storeAs nm val >> return sa
    qReply (StoreAs nm val) = qReply val
    qFilterSuccess (StoreAs _ xs) = qFilterSuccess xs

data StoreAs a = StoreAs String [a]

x @= y = StoreAs x y


inLastSession :: StateT QState IO a -> IO a
inLastSession sma = do
  s <- lastSession "/var/bugpan/sessions/"
  fst `fmap`  (runStateT sma $ QState s 0 0 True)

inSession :: Session -> StateT QState IO a -> IO a
inSession s sma =  fst `fmap`  (runStateT sma $ QState s 0 0 True)


inNewSession :: StateT QState IO a -> IO a
inNewSession sma = do sess <- newSession "/var/bugpan/sessions/"
                      inSession sess sma

inTemporarySession sma = do sess <- newSession "/var/bugpan/sessions/"
                            inSession sess sma
                            deleteSession sess

inSessionNamed :: String -> StateT QState IO a -> IO a
inSessionNamed nm sma = do sess <- loadExactSession $ "/var/bugpan/sessions/"++nm
                           inSession sess sma


inApproxSession :: String -> StateT QState IO a -> IO a
inApproxSession nm sma = do sess <- loadApproxSession "/var/bugpan/sessions/" nm
                            inSession sess sma

inEverySessionIn :: StateT QState IO a -> IO [a]
inEverySessionIn sma = do
  sessNms <- getSessionInRootDir "/var/bugpan/sessions/"
  sessns <- mapM (loadExactSession . ("/var/bugpan/sessions/"++)) sessNms
  forM sessns $ \s -> inSession s sma


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

{-testQ = do s <- lastSession "/var/bugpan/sessions/"
           print s
           qres <- runAskM s $ signals "vm"
           mapM plotWithR qres
                         
           --mapM print qres
           return () -}