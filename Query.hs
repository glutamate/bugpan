{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, NoMonomorphismRestriction #-} 

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
import System.Random.Mersenne
import Data.Char

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
import Data.Unique
import qualified Data.Binary as B
import System.Environment
import System.Console.Readline
import System.IO
 
double :: Double
double = undefined


--change these to loadUntyped
signals :: (MonadIO m, Reify a) => String -> a-> StateT QState m [Signal a]
signals nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/signals/"++nm++"/"++fn 
          return . catMaybes $ map reify $ concat sigs) 
      (return [])

signalsDirect :: MonadIO m => String -> StateT QState m [Signal Double]
signalsDirect nm = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadSignalsU $ bdir++"/signals/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])


events :: (MonadIO m, Reify (Event a)) => String -> a-> StateT QState m [Event a]
events nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/events/"++nm++"/"++fn
          return . catMaybes $ map reify $ concat utevs) 
      (return [])

eventsDirect :: (MonadIO m, LoadDirectly [(Double,a)]) => String -> a-> StateT QState m [Event a]
eventsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/events/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])

durations :: (MonadIO m, Reify (Duration a)) => String -> a-> StateT QState m [Duration a]
durations nm _ = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          return . catMaybes $ map reify $ concat eps)
      (return [])            

unitDurations :: MonadIO m => String -> StateT QState m [Duration ()]
unitDurations nm = do
  Session bdir t0 <- getSession
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          return $ map reifyIt $ concat eps)
      (return [])            
      where reifyIt (PairV (PairV t1 t2) v) = ((unsafeReify t1, unsafeReify t2), ())

durationsDirect :: (MonadIO m, LoadDirectly [(Double,a)]) => String -> a-> StateT QState m [Event a]
durationsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/durations/"++nm++"/"++fn 
          return $ concat sigs) 
      (return [])

data StoreMode = OverWrite | NoOverWrite | Append

storeAs :: MonadIO m => Reify a => String -> [a] -> StateT QState m [a]
storeAs = storeAs' NoOverWrite

storeAsOvwrt :: (MonadIO m, Reify a) => String -> [a] -> StateT QState m [a]
storeAsOvwrt = storeAs' OverWrite

storeAsAppend = storeAs' Append

storeAs' _ _ []  =return []
storeAs' ovwrt nm vls = do 
  let vs = map pack vls
  let t = typeOfReified $ head vls
  let uniqueIntStr = (show. hashUnique) `fmap` newUnique
  suffix <- liftIO $ uniqueIntStr
  --liftIO $ print vs
  let subDir = case t of
                 SignalT _ -> "signals/"
                 PairT (NumT (Just RealT)) _ -> "events/"
                 PairT (PairT (NumT (Just RealT)) (NumT (Just RealT))) _ -> "durations/"
                 _ -> error $ "cannot store "++nm++": unknown type" 
  Session bdir t0 <- getSession
  case ovwrt of
     OverWrite -> liftIO $ do
       whenM (doesDirectoryExist $ bdir ./ subDir ./ nm)
             (removeDirectoryRecursive $ bdir ./ subDir ./ nm)
       createDirectory $ bdir ./ subDir ./ nm
       saveVs (oneTrailingSlash bdir++subDir++nm++"/stored"++suffix) vs
     NoOverWrite -> liftIO $ do
       whenM (not `fmap` (doesDirectoryExist $ bdir ./ subDir ./ nm))
             (do createDirectory $ bdir ./ subDir ./ nm
                 saveVs (oneTrailingSlash bdir++subDir++nm++"/stored"++suffix) vs)
     Append -> liftIO $ do
                 createDirectoryIfMissing False $ bdir ./ subDir ./ nm
                 saveVs (oneTrailingSlash bdir++subDir++nm++"/stored"++suffix) vs
  return vls

deleteValue nm = do Session bdir t0 <- getSession
                    forM_ ["signals", "", ""] $ \kind -> 
                        whenM (liftIO $ doesDirectoryExist $ bdir ./ kind ./ nm)
                                  (liftIO $ removeDirectoryRecursive $ bdir./kind./nm)


exists :: MonadIO m =>String -> StateT QState m Bool
exists  nm =   do   Session bdir t0 <- getSession
                    or `fmap` sequence [liftIO $ doesDirectoryExist $ bdir ./ "signals" ./ nm,
                                        liftIO $ doesDirectoryExist $ bdir ./ "events" ./ nm,
                                        liftIO $ doesDirectoryExist $ bdir ./ "durations" ./ nm]
                             

instance (Reify a, QueryResult [a]) => QueryResult (StoreAs a) where
    qResThroughSession sa@(StoreAs nm val False) = storeAs nm val >> return sa
    qResThroughSession sa@(StoreAs nm val True) = storeAsOvwrt nm val >> return sa
    qReply (StoreAs nm val _) = qReply val
    qFilterSuccess (StoreAs _ xs _) = qFilterSuccess xs

data StoreAs a = StoreAs String [a] Bool

x @= y = StoreAs x y False
x @=! y = StoreAs x y True


--inLastSession :: MonadIO m => StateT QState IO a -> IO a
inLastSession sma = do
  s <- liftIO $ lastSession "/var/bugpan/sessions/"
  inSession s sma
  --fst `fmap`  (runStateT sma $ QState s 0 0 True)

inSession :: (MonadIO m, Functor m) => Session -> StateT QState m a -> m a
inSession s sma =  do args <- liftIO $ getArgs
                      gen <- liftIO $ getStdGen
                      rnds <- liftIO $ randoms gen
                      fst `fmap`  (runStateT sma $ QState s 0 0 True args Nothing rnds)

inSessionFromArgs sma = do allargs <- liftIO $ getArgs
                           let (opts, nm:args) = partition beginsWithHyphen allargs
                           sess <- liftIO $ loadApproxSession "/var/bugpan/sessions/" nm
                           fst `fmap`  (runStateT sma $ QState sess 0 0 True (opts++args) Nothing)


--inNewSession :: StateT QState IO a -> IO a
inNewSession sma = do sess <- liftIO $ newSession "/var/bugpan/sessions/"
                      inSession sess sma

--inNewSessionWith :: String -> ClockTime -> StateT QState IO a -> IO a
inNewSessionWith nm t0 sma = do sess <- liftIO $ createSession "/var/bugpan/sessions/" t0 nm
                                inSession sess sma


inTemporarySession sma = do sess <- liftIO $ newSession "/var/bugpan/sessions/"
                            inSession sess sma
                            liftIO $ deleteSession sess

--inSessionNamed :: String -> StateT QState IO a -> IO a
inSessionNamed nm sma = do sess <- liftIO $ loadExactSession $ "/var/bugpan/sessions/"++nm
                           inSession sess sma


--inApproxSession :: String -> StateT QState IO a -> IO a
inApproxSession nm sma = do sess <- liftIO $ loadApproxSession "/var/bugpan/sessions/" nm
                            inSession sess sma

--inEverySession :: StateT QState IO a -> IO [a]
inEverySession sma = do
  sessNms <- liftIO $ getSessionInRootDir "/var/bugpan/sessions/"
  sessns <- liftIO $ mapM (loadExactSession . ("/var/bugpan/sessions/"++)) sessNms
  forM sessns $ \s -> inSession s sma


--sessionTmax  ::  StateT QState IO RealNum
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

io = liftIO 

undefinedPerformQuery :: MonadIO m => StateT QState m a -> a
undefinedPerformQuery = error $ "undefinedPerformQuery"

initUserInput = liftIO $ do --hSetBuffering stdin NoBuffering 
                            initialize

userValue :: (Reify a, Read a) => String -> StateT QState IO a
userValue q = res 
    where tyNm = ppType $ typeOfReified $ undefinedPerformQuery res
          res = do
            maybeLine <- liftIO $ readline $ q++" ::"++tyNm++"> "            
            case maybeLine of 
              Nothing    -> fail "userValue: readline fail" --return () -- EOF / control-d
              Just line ->  case safeRead line of
                              Just n -> return $ n 
                              Nothing -> do liftIO $putStrLn $ "not a "++tyNm
                                            userValue q
      
readChar = do {-c <- getChar
              if c== '\n' || c =='\r'
                 then readChar
                 else return c -}
  mline <- liftIO $ readline ""
  case mline of
    Nothing -> fail "readChar"
    Just [] -> readChar
    Just (c:cs) -> return c

userChoice :: [(Char, String, StateT QState IO a)] ->  StateT QState IO a
userChoice opts = do
  forM_ opts $ \(c, s,_) -> liftIO $ putStrLn $ (c:": ")++s
  liftIO $ hFlush stdout
  choice <- liftIO $ readChar
  case find (\(c, _,_) -> c==choice) opts of
    Nothing -> do liftIO $ putStrLn $ "invalid choice: "++(choice:"")
                  userChoice opts
    Just (_,_,x) -> x

userConfirm :: String -> StateT QState IO Bool
userConfirm s = do liftIO $ putStr $ s++ " (y/N)? "
                   liftIO $ hFlush stdout
                   choice <- liftIO $ readChar
                   return $ toLower choice == 'y'


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