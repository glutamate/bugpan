{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, UndecidableInstances, NoMonomorphismRestriction, CPP #-} 

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
--import System.Console.Readline
import System.IO
import Math.Probably.Student
import Math.Probably.FoldingStats
import Text.Printf
import NewSignal
import System.Info
 
double :: Double
double = undefined


bugpanRootDir = if os == "mingw32"
                   then "c:/bugdir/" 
                   else "/var/bugpan/" 

sessionsRootDir = bugpanRootDir./"sessions/"

--change these to loadUntyped
signals :: (MonadIO m, Reify (Signal a)) => String -> a-> StateT QState m [Signal a]
signals nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/signals/"++nm++"/"++fn 
          return . shift tshift $ catMaybes $ map reify $ concat sigs) 
      (return [])

signalsDirect :: MonadIO m => String -> StateT QState m [Signal Double]
signalsDirect nm = do
  Session bdir t0 <- getSession
  tshift <- loadShift `fmap` get
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadSignalsU $ bdir++"/signals/"++nm++"/"++fn 
          return $ shift tshift $ concat sigs) 
      (return [])


events :: (MonadIO m, Reify (Event a)) => String -> a-> StateT QState m [Event a]
events nm _ = do
  Session bdir t0 <- getSession
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          utevs <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/events/"++nm++"/"++fn
          return . shift tshift $ catMaybes $ map reify $ concat utevs) 
      (return [])

eventsDirect :: (MonadIO m, LoadDirectly [(Double,a)]) => String -> a-> StateT QState m [Event a]
eventsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/events/"++nm++"/"++fn 
          return $ shift tshift $ concat sigs) 
      (return [])

durations :: (MonadIO m, Reify (Duration a)) => String -> a-> StateT QState m [Duration a]
durations nm _ = do
  Session bdir t0 <- getSession
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          return . shift tshift $ catMaybes $ map reify $ concat eps)
      (return [])            

unitDurations :: MonadIO m => String -> StateT QState m [Duration ()]
unitDurations nm = do
  Session bdir t0 <- getSession
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadVs $ bdir++"/durations/"++nm++"/"++fn
          return $ shift tshift $ map reifyIt $ concat eps)
      (return [])            
      where reifyIt (PairV (PairV t1 t2) v) = ((unsafeReify t1, unsafeReify t2), ())

unitDurationsStrict :: MonadIO m => String -> StateT QState m [Duration ()]
unitDurationsStrict nm = do
  Session bdir t0 <- getSession
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          eps <- forM fnms $ \fn-> liftIO $ loadBinaryStrict $ bdir++"/durations/"++nm++"/"++fn
          return $ shift tshift $ map reifyIt $ concat eps)
      (return [])            
      where reifyIt (PairV (PairV t1 t2) v) = ((unsafeReify t1, unsafeReify t2), ())


durationsDirect :: (MonadIO m, LoadDirectly [(Double,a)]) => String -> a-> StateT QState m [Event a]
durationsDirect nm _ = do
  Session bdir t0 <- getSession
  --liftIO . print $ bdir++"/signals/"++nm
  tshift <- loadShift `fmap` get
  ifM (liftIO (doesDirectoryExist (bdir++"/durations/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/durations/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadDirectly $ bdir++"/durations/"++nm++"/"++fn 
          return $ shift tshift $ concat sigs) 
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
                 allfnms <- getSortedDirContents $ bdir ./ subDir ./ nm
                 let fileNoMax = foldl (max) 0 $ catMaybes $ map (safeRead . (drop 6)) $ filter ("stored" `isPrefixOf`) allfnms
                 saveVs (oneTrailingSlash bdir++subDir++nm++"/stored"++show (fileNoMax + idInt 1)) vs
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

 
ttest :: (MonadIO m, Functor m) => StateT QState m (Double, Double) -> m String
ttest getvls = do
  vls <- inEverySession getvls
  let tval = runStat pairedSampleT vls
  let df =  length vls - 1
  let pval =  (1-) $ studentIntegral (tval) (realToFrac df)
  return $ printf "paired t(%d)=%.3g, p=%.3g" df tval pval 

ttest1 :: (MonadIO m, Functor m) => StateT QState m Double -> m String
ttest1 getvls = do
  vls <- inEverySession getvls
  let tval = runStat (oneSampleT 0) vls
  let df =  length vls - 1
  let pval =  (1-) $ studentIntegral (tval) (realToFrac df)
  return $ printf "one-sample t(%d)=%.3g, p=%.3g" df tval pval 


inLastSession :: (MonadIO m, Functor m) => StateT QState m a -> m a
inLastSession sma = do
  s <- liftIO $ lastSession sessionsRootDir
  inSession s sma
  --fst `fmap`  (runStateT sma $ QState s 0 0 True)

inSession :: (MonadIO m, Functor m) => Session -> StateT QState m a -> m a
inSession s sma =  do args <- liftIO $ getArgs
                      gen <- liftIO $ getStdGen
                      rnds <- liftIO $ randoms gen
                      fst `fmap`  (runStateT sma $ QState s 0 0 True args Nothing rnds False False 0)

inSessionFromArgs :: (MonadIO m, Functor m) => StateT QState m a -> m a
inSessionFromArgs sma = do allargs <- liftIO $ getArgs
                           let (opts, nm:args) = partition beginsWithHyphen allargs
                           sess <- liftIO $ loadApproxSession sessionsRootDir nm
                           gen <- liftIO $ getStdGen
                           rnds <- liftIO $ randoms gen
                           fst `fmap`  (runStateT sma $ QState sess 0 0 True (opts++args) Nothing rnds False False 0)


inNewSession :: (MonadIO m, Functor m) => StateT QState m a -> m a
inNewSession sma = do sess <- liftIO $ newSession sessionsRootDir
                      inSession sess sma

inNewSessionWith :: (MonadIO m, Functor m) => String -> ClockTime -> StateT QState m a -> m a
inNewSessionWith nm t0 sma = do sess <- liftIO $ createSession sessionsRootDir t0 nm
                                inSession sess sma

inTemporarySession :: (MonadIO m, Functor m) => StateT QState m a -> m a
inTemporarySession sma = do sess <- liftIO $ newSession sessionsRootDir
                            res <- inSession sess sma
                            liftIO $ deleteSession sess
                            return res

inSessionNamed :: (MonadIO m, Functor m) => String -> StateT QState m a -> m a
inSessionNamed nm sma = do sess <- liftIO $ loadExactSession $ sessionsRootDir++nm
                           inSession sess sma


inApproxSession :: (MonadIO m, Functor m) => String -> StateT QState m a -> m a
inApproxSession nm sma = do sess <- liftIO $ loadApproxSession sessionsRootDir nm
                            inSession sess sma

inEverySession :: (MonadIO m, Functor m) => StateT QState m a -> m [a]
inEverySession sma = do
  sessNms <- liftIO $ getSessionInRootDir sessionsRootDir
  sessns <- liftIO $ mapM (loadExactSession . (sessionsRootDir++)) sessNms
  forM sessns $ \s -> inSession s sma

inEverySession_ :: (MonadIO m, Functor m) => StateT QState m a -> m ()
inEverySession_ sma = do
  sessNms <- liftIO $ getSessionInRootDir sessionsRootDir
  --liftIO $ mapM print sessNms
--  sessns <- liftIO $ mapM (loadExactSession . (sessionsRootDir++)) sessNms
  forM_ sessNms $ \snm -> do 
    --ftIO $ print $ snm++" start"
    inSessionNamed snm sma
    --liftIO $ print $ snm++" done "

deleteSessionIfExists :: String -> IO ()
deleteSessionIfExists nm = 
    whenM (existsSession nm sessionsRootDir) $ 
      loadApproxSession sessionsRootDir nm >>= deleteSession



loopM :: Monad m => ((a, b) -> m (a, c)) -> a -> [b] -> m [c] 
loopM f x0 [] = return []
loopM f x0 (y:ys) = do (x1, z) <- f (x0,y)
                       (z:) `liftM` loopM f x1 ys
                       --return $ z:rest

whenMaybe :: Monad m => Bool -> m a -> m (Maybe a)
whenMaybe True  = liftM Just
whenMaybe False =  const (return Nothing)

manySessionData :: (MonadIO m, Functor m) => StateT QState m (Maybe a) -> m [a]
manySessionData sma = do -- catMaybes `fmap` inEverySession (setup >> ma)
  sessNms <- liftIO $ getSessionInRootDir sessionsRootDir
  --sessns <- liftIO $ mapM (loadExactSession . (sessionsRootDir++)) sessNms
  ress <- loopM setup 0 sessNms -- no, here increment lastTStop
  return $ catMaybes ress
    where setup (lastTStop, sNm) = inSessionNamed sNm $ do
            running <- durations "running" ()
            let t2s = foldr (max) minBound $ map (snd . fst) running
            --lastLastTStop <- lastTStop `fmap` get
            modify (\s -> s {loadShift = lastTStop})
            --io $ print (sNm,lastTStop + 60) 
            res <- sma
            if isJust res
               then return (lastTStop + 60 + t2s, res) 
               else return (lastTStop, res) 

sessionDur :: (MonadIO m, Functor m) => StateT QState m [Duration String]
sessionDur = do
  snm <- getSessionName
  fmap (map (\(ts,v) -> (ts, snm))) $ oneDur `fmap` durations "running" ()
  
oneDur :: [Duration a] -> [Duration ()]
oneDur durs = (runStat (before minF fst `both` before maxF snd) $ map fst durs, ()):[]


inEverySessionWhere :: (MonadIO m, Functor m) => StateT QState m Bool -> StateT QState m a -> m [a]
inEverySessionWhere mfilt sma = do
  sessNms <- liftIO $ getSessionInRootDir sessionsRootDir
  sessns <- liftIO $ mapM (loadExactSession . (sessionsRootDir++)) sessNms
  res <- forM sessns $ \s -> inSession s $ do
                                  b<- mfilt 
                                  if b then Just `fmap` sma else return Nothing
  return $ catMaybes res

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

{-

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


-}

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

{-testQ = do s <- lastSession sessionsRootDir
           print s
           qres <- runAskM s $ signals "vm"
           mapM plotWithR qres
                         
           --mapM print qres
           return () -}
