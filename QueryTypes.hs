{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, IncoherentInstances, DeriveDataTypeable, NoMonomorphismRestriction, BangPatterns, TypeOperators, GADTs #-} 

module QueryTypes where

import EvalM hiding (ListT)
--import Eval
--import Expr
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
--import Traverse
--import Transform
--import Stages
import Data.Ord
--import Charts
import Control.Concurrent
import Database
import HaskSyntaxUntyped
import Data.Maybe
--import Math.Probably.PlotR
import Data.Unique
import TNUtils
import Data.Typeable
import Math.Probably.FoldingStats
import Math.Probably.Sampler
import System.IO
import ValueIO
--import Array
import qualified Data.StorableVector as SV
import Foreign.Storable
--import System.Posix.Files
import PlotGnuplot
import Text.Regex.Posix
import Text.Printf
import NewSignal
--import Graphics.Rendering.HSparklines
import System.Environment
import qualified Data.Binary as B

type Duration a = ((Double,Double),a)
type Event a = (Double,a)

data QState = QState { qsSess:: Session,
                       lastTStart:: Double,
                       lastTStop :: Double,
                       realTime :: Bool,
                       shArgs :: [String],
                       remoteCmdFile :: Maybe String,
                       rnds :: Seed,
                       forLiterate :: Bool,
                       forTable :: Bool,
                       loadShift :: Double
                     } deriving Show

type QueryM = StateT QState IO

sampleNQ :: MonadIO m => Int -> Sampler a -> StateT QState m [a]
sampleNQ n sf = do
  rans <- rnds `liftM` get
--  modify $ \s-> s {rnds = []}
  let (vls, rans') = sam n rans sf []
  modify $ \s-> s {rnds = rans'}
  return vls
    where sam 0 rs _ xs          = (xs, rs)
          sam n rs s@(Sam sf) xs = let (x, rs') = sf rs 
                                   in sam (n-1) rs' s (x:xs)

sampleQ :: MonadIO m => Sampler a -> StateT QState m a
sampleQ (Sam sf) = do
  rans <- rnds `liftM` get
  let (x, rans') = sf rans
  modify $ \s-> s {rnds = rans'}
  return $ x 

 
getSession = qsSess `liftM` get
getSessionName = do Session bdir _ <- getSession
                    return $ last $ splitBy '/' bdir



getSessionStart =  do
   Session bdir _ <- getSession
   (t1,t2) <- liftIO $ read `fmap` readFile (bdir++"/tStart")
   return $  TOD t1 t2



openReplies = modify (\s-> s { shArgs = "-o" : shArgs s })
plotSize w h = modify (\s-> s { shArgs = ("-h"++show h) : ("-w"++show w): shArgs s })
fontSize f= modify (\s-> s { shArgs = ("-f"++show f) : shArgs s })

setForLiterate = modify (\s-> s { forLiterate = True})
setForTable = modify (\s-> s { forTable = True})
setNotForTable = modify (\s-> s { forTable = False})

isForLiterate = forLiterate `fmap` get
isForTable = forTable `fmap` get

--zipWithTime :: Signal a -> Signal (a,Double)
--zipWithTime (Signal t1 t2 dt sf) = Signal t1 t2 dt $ \pt -> (sf pt, (realToFrac pt)*dt+t1)



during :: ChopByDur [t] => [Duration b] -> [t] -> [t]
during durs evs = concat $ chopByDur durs evs

--concatMap (during' evs) durs
--    where during' evs dur = filter (`evInDuration` dur) evs


section :: [Signal a] -> [Duration b] -> [Signal a]
section sigs = concatMap f where
  f ((t1,t2),_) = catMaybes $ map (limitSig' t1 t2) sigs


--(durs) = map (snd. snd) $ sectionGen sigs durs {-case find (sigOverlapsDur dur) sigs of
--                            Just sig -> section1 sig dur : section sigs durs
--                            Nothing -> section sigs durs-}

sectionGen :: [Signal a] -> [Duration b] -> [Duration (b,Signal a)]
sectionGen _ [] = []
sectionGen sigs (dur@(ts,v):durs) = (map f $ takeWhile (sigOverlapsDur dur) $
                                         dropWhile (not . sigOverlapsDur dur) 
                                         sigs) ++ sectionGen sigs durs
    where f sig = (ts, (v,section1 sig dur)) 
                           


sectionDur1 :: Duration a -> [Duration b] -> [Duration b]
sectionDur1 ((lo,hi),_) durs = concatMap f durs
    where f ((t1,t2),v) | t2 <lo || t1 > hi = []
                        | otherwise = [((max lo t1, min hi t2), v)]


section1 :: Signal a -> ((Double, Double), t) -> Signal a
section1 sig ((td1,td2),vd) = limitSig td1 td2 sig

sigContainsDur :: Duration b -> Signal a -> Bool
sigContainsDur ((td1,td2),vd) (Signal ts1 ts2 dt sf _) = ts1 < td1 && ts2 > td2

sigOverlapsDur :: Duration b -> Signal a -> Bool
sigOverlapsDur ((td1,td2),vd) (Signal ts1 ts2 dt sf _) = td2 > ts1 && td1<ts2 -- || td1 < ts2 && td2 >ts1

durOverlapsDur :: Duration b -> Duration a -> Bool
durOverlapsDur ((td1,td2),vd) ((ts1,ts2),vd1) = td2 > ts1 && td1<ts2 -- || td1 < ts2 && td2 >ts1


type List a = [a]
type Id a = a

vToEvent v = (evTime v, evTag v)
vToDuration v = let (t1, t2) = epTs v in ((t1, t2), epTag v)

evInDuration (t,_) ((t1,t2), _) = t<t2 && t>t1



showDur ((t1,t2),v) = show t1 ++ ".."++show t2++": "++show v
showEvt (t,v) = "@"++show t++": "++show v

sigDur :: Signal a -> Duration ()
sigDur (Signal t1 t2 _ _ _) = ((t1,t2), ())



sigSegStat :: (Storable a) => Fold a b -> (Int, Int) -> Signal a -> b
sigSegStat (F op init c _) (n1, n2) sig = 
    let arr = SV.take (n2-n1) $ SV.drop n1 $ sigArr sig
    in c $! SV.foldl' op init arr


class HasTStart t where
    gettStart :: t a -> Double

instance HasTStart ((,) Double) where
    gettStart = getTStart

instance HasTStart ((,) (Double, Double)) where
    gettStart = getTStart

instance HasTStart Signal where
    gettStart (Signal t1 _ _ _ _) = t1

class Tagged t where
    getTag :: t a-> a
    setTag :: t a-> b ->t b
    getTStart :: t a -> Double
    getTStop :: t a -> Double

instance Tagged ((,) Double) where
    getTag = snd
    setTag (t,_) v = (t,v)
    getTStart (t,_) = t
    getTStop (t,_) = t

instance Tagged ((,) (Double, Double)) where
    getTag (_,v) = v
    setTag ((t1,t2),_) v = ((t1,t2), v)
    getTStart ((t1,t2),_) = t1
    getTStop ((t1,t2),_) = t2

foldTagged ::  Tagged t => (a -> b -> a) -> a -> [t b] -> a
foldTagged f init col = foldl' f init $ map getTag col

sumTags = sum . map getTag


--instance Functor ((,) Double) where
--    fmap f (t,v) = (t, f v)

{-instance Functor ((,) (Double, Double)) where
    fmap f ((t1,t2), v) = ((t1,t2),f v) -}
 

instance Shiftable (Double,a) where
    shift ts (t,v) = (t+ts, v)
    rebaseTime tr (t,v) = (t*tr, v) 
instance Shiftable ((Double,Double),a) where
    shift ts ((t1,t2),v) = ((t1+ts,t2+ts),v)
    rebaseTime tr ((t1,t2),v) = ((t1*tr, t2*tr), v) 

instance Shiftable (Signal a) where
    shift ts (Signal t1 t2 dt sf eok) = Signal (t1+ts) (t2+ts) dt sf eok
    rebaseTime tr (Signal t1 t2 dt sf eok) = Signal (t1*tr) (t2*tr) (dt*tr) sf eok
 
instance Shiftable a => Shiftable [a] where
    shift ts vls = map (shift ts) vls
    rebaseTime t = map (rebaseTime t)

instance (Shiftable a, Shiftable b) => Shiftable (a,b) where
    shift ts (x,y) = (shift ts x, shift ts y)
    rebaseTime t (x,y) = (rebaseTime t x, rebaseTime t y)


individually :: ListT m a -> m [a]
individually = runListT

eachOf :: Monad m => [a] -> ListT m a
eachOf xs = ListT . return $ xs

ask :: (QueryResult a, MonadIO m) => a -> StateT QState m String
ask qx = do
  x <- qResThroughSession qx
  args <- shArgs `liftM` get
  qos <- liftIO $ qReply x args
  --let str = unlines $ [s | QString s <- qos ]
  liftIO $ putStrLn $ qos
  return qos

--askForLiterate :: (QueryResult a, MonadIO m) => a ->  m ()
askForLiterate qx = do
--  modify (\s-> s { shArgs = "-litlatex" : shArgs s })
--  x <- qResThroughSession qx
  args <- shArgs `fmap` get
  str <- liftIO $ qReply qx ("-litlatex" : args)
  --let str = unlines $ [s | QString s <- qos ]
  --liftIO $ putStr "askForLiterate"
  --liftIO $ putStrLn $ str
  cond [(str =~ "file://(.+)\\.html", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.html"
                  lns <- lines `fmap` (readFile $ s++".html")
                  putStr $ unlines $ map (substitute "style=\"float: left\"" "") lns),
        (str =~ "file://(.+)\\.png", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.png"
                  putStr $ "<img src=\""++ s++".png\" />")
       ] $ (liftIO $ putStrLn $ noTypeLine str)
  return ()

askForLiterateIO :: (QueryResult a, MonadIO m) => a -> m ()
askForLiterateIO qx = do
  str <- liftIO $ qReply qx ["-litlatex"]
  --let str = unlines $ [s | QString s <- qos ]
  --liftIO $ putStr "askForLiterate"
  --liftIO $ putStrLn $ str
  cond [(str =~ "file://(.+)\\.html", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.html"
                  lns <- lines `fmap` (readFile $ s++".html")
                  putStr $ unlines $ map (substitute "style=\"float: left\"" "") lns),
        (str =~ "file://(.+)\\.png", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.png"
                  putStr $ "<img src=\""++ s++".png\" />")
       ] $ (liftIO $ putStrLn $ noTypeLine str)
  return ()


noTypeLine s = unlines $ ntl $ lines s
    where ntl ls@(ln:lns) | "type = " `isPrefixOf` ln = lns
                          | otherwise = ls
          ntl [] = []

askForLiterateTable :: (QueryResult a, MonadIO m) => a -> StateT QState m ()
askForLiterateTable qx = do
  modify (\s-> s { shArgs = "-litlatex" :"-g" : shArgs s })
  x <- qResThroughSession qx
  args <- shArgs `liftM` get
  str <- liftIO $ qReply x args
  --let str = unlines $ [s | QString s <- qos ]
  --liftIO $ putStrLn $ qos
  cond [(str =~ "file://(.+)\\.html", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.html"
                  lns <- lines `fmap` (readFile $ s++".html")
                  putStr $ unlines $ map (substitute "style=\"float: left\"" "") lns),
        (str =~ "file://(.+)\\.png", liftIO $ do
                  let [[_, s]] = str =~ "file://(.+)\\.png"
                  putStr $ "<img src=\""++ s++".png\" />")
       ] $ (liftIO $ putStr $ str)
  modify (\s-> s { shArgs = tail $ shArgs s })
  return ()


isSingle [x] = True
isSingle _ = False

grid opts = "-g" `elem` opts 

data QueryResultBox = forall a. QueryResult a => QResBox a deriving Typeable

data QOutcome = QString String | QPlot GnuplotBox

class QueryResult a where
    qReply :: a -> [String] -> IO String -- [QOutcome]
    qResThroughSession :: MonadIO m => a -> StateT QState m a
    qResThroughSession = return 
    qFilterSuccess :: a -> Bool
    qFilterSuccess = const True

qs1 s = s -- [QString s]
class Saveable a where
    showLine :: a -> String

data SaveArray a = SaveArray String [a]

instance Saveable Double where
   showLine = show

instance (Saveable a, Saveable b) => Saveable (a,b) where
   showLine (x,y) = showLine x ++ "\t" ++ showLine y

instance Saveable a => Saveable [a] where
   showLine xs = intercalate"\t" $ map showLine xs

instance Saveable a => QueryResult (SaveArray a) where
     qReply (SaveArray nm xs) _ = do
         withFile nm WriteMode $ \h -> do
            forM_ xs $ hPutStrLn h . showLine
         return ""
      
data SaveSignals = SaveSignals String [Signal Double]


instance B.Binary SaveSignals where
   put (SaveSignals _ sigs) = do
     B.put (length sigs)
     forM_ (map forceSigEq sigs) putSig 


   get = do n <- B.get
            sigs <- forM [1..(n::Int)] $ const $ do
               dt <- B.get
               t0 <- B.get
               npnts <- B.get
               let t2 = t0+(realToFrac npnts)*dt
               pts <- forM [1..(npnts::Int)] $ const $ B.get
               return $ Signal t0 t2 dt (SV.pack pts) Eq
            return $ SaveSignals "loaded" sigs

putSig :: Signal Double -> B.Put 
putSig (Signal t1 t2 dt arr Eq) = do
       B.put dt
       B.put t1
       B.put $ SV.length arr
       forM_ (SV.unpack arr) $ B.put
       return()

instance QueryResult ClockTime where
     qReply tod _ = return $ show tod

instance QueryResult SaveSignals where
     qReply s@(SaveSignals nm xs) _ = do
         B.encodeFile nm s
--         withFile nm WriteMode $ \h -> do
--            forM_ xs $ hPutStrLn h . showLine
         return ""


instance QueryResult [Char] where
    qReply s _ = return $ qs1 s
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance QueryResult Int where
    qReply x _ = return .qs1$ printf "%.3d" x
    qFilterSuccess 0 = False
    qFilterSuccess _ = True

instance QueryResult [Double] where
    qReply xs _ = return .qs1 $ show xs
    qFilterSuccess = not . null

instance QueryResult Double where
    qReply x _ = return . qs1 $ show x
    qFilterSuccess 0 = False
    qFilterSuccess _ = True


class ChopByDur a where
    chopByDur :: [Duration b] -> a -> [a]

instance ChopByDur [Signal a] where
    chopByDur durs sigs = map (\dur->section sigs [dur]) durs

instance ChopByDur [Event a] where
    chopByDur durs evs = map (\((t1,t2),_)->filter ((\t->t>t1 && t<t2 ). fst) evs) durs

instance ChopByDur [Duration a] where
    chopByDur chopDurs durs = map (\dur->sectionDur1 dur durs) chopDurs

instance (ChopByDur a, ChopByDur b) => ChopByDur (a,b) where
    chopByDur durs (xs, ys) = zip (chopByDur durs xs) (chopByDur durs ys)

instance ChopByDur [Double] where --list of times
    chopByDur durs dbls = map (\((t1,t2),_)->filter ((\t->t>t1 && t<t2 )) dbls) durs


chopAndReset :: (ChopByDur a, Shiftable a) => [Duration b] -> a -> [a]
chopAndReset durs evs = map (\(xs, ((t1,t2),v))-> shift (negate t1) xs) $ zip (chopByDur durs evs) durs


downSample n = map (downSample' (n `div` 2))

downSample' :: (Ord a, Bounded a, Num a, Storable a) => Int -> Signal a -> Signal a
--downSample' :: Int -> Signal Double -> Signal Double
downSample' n sig@(Signal t1 t2 dt arr _) =
    let x ./. y = realToFrac x / realToFrac y
        npw = SV.length arr --round $ (t2-t1)/dt
        chunkSize = floor (npw./. n)
        nChunks =  ceiling (npw ./. chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = SV.pack $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)/(realToFrac $ nChunks*2))  narr Eq)
           else sig
downSample' n s = error $ "querytypes.downSample': " ++ show s

downsample n = map (downsample' n)
downsample' :: (Bounded a, Num a, Storable a, Fractional a) => Int -> Signal a -> Signal a
--downSample' :: Int -> Signal Double -> Signal Double
downsample' n sig@(Signal t1 t2 dt _ _) =
    let x ./. y = realToFrac x / realToFrac y
        npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./. n)
        nChunks =  ceiling (npw ./. chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks)
        narr = SV.pack $map chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x) = sigSegStat (meanF) (n1,n2) sig
                      in x
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)/(realToFrac nChunks))  narr Eq)
           else sig
