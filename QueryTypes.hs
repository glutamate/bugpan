{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, IncoherentInstances, DeriveDataTypeable, NoMonomorphismRestriction, BangPatterns, TypeOperators #-} 

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
import System.Posix.Files
import PlotGnuplot
import Text.Regex.Posix

--import Graphics.Rendering.HSparklines

type Duration a = ((Double,Double),a)
type Event a = (Double,a)

data QState = QState { qsSess:: Session,
                       lastTStart:: Double,
                       lastTStop :: Double,
                       realTime :: Bool,
                       shArgs :: [String],
                       remoteCmdFile :: Maybe String,
                       rnds :: [Double]
                     } deriving Show

type QueryM = StateT QState IO

{-sampleN :: Int -> Sampler a -> QueryM [a]
sampleN n sf = do
  rans <- rnds `fmap` get
  modify $ \s-> s {rnds = []}
  let (vls, rans') = sam n rans sf []
  modify $ \s-> s {rnds = rans'}
  return vls
    where sam 0 rs _ xs          = (xs, rs)
          sam n rs s@(Sam sf) xs = let (x, rs') = sf rs 
                                   in sam (n-1) rs' s (x:xs)

sample :: Sampler a -> QueryM a
sample (Sam sf) = do
  rans <- rnds `fmap` get
  let (x, rans') = sf rans
  modify $ \s-> s {rnds = rans'}
  return $ x -}
 
getSession = qsSess `fmap` get
getSessionName = do Session bdir _ <- getSession
                    return $ last $ splitBy '/' bdir

openReplies = modify (\s-> s { shArgs = "-o" : shArgs s })
plotSize w h = modify (\s-> s { shArgs = ("-h"++show h) : ("-w"++show w): shArgs s })


zipWithTime :: Signal a -> Signal (a,Double)
zipWithTime (Signal t1 t2 dt sf) = Signal t1 t2 dt $ \pt -> (sf pt, (realToFrac pt)*dt+t1)


sigInitialVal (Signal t1 t2 dt sf) = sf 0

foldSig :: (a->b->a) -> a -> Signal b -> a
foldSig f init sig = foldl' f init $ sigToList sig


during :: ChopByDur [t] => [Duration b] -> [t] -> [t]
during durs evs = concat $ chopByDur durs evs

--concatMap (during' evs) durs
--    where during' evs dur = filter (`evInDuration` dur) evs


section :: [Signal a] -> [Duration b] -> [Signal a]
section _ [] = []
section sigs (durs) = map (snd. snd) $ sectionGen sigs durs {-case find (sigOverlapsDur dur) sigs of
                            Just sig -> section1 sig dur : section sigs durs
                            Nothing -> section sigs durs-}

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
section1 (Signal ts1 ts2 dt sf) ((td1,td2),vd) = let (t1, t2)= (max ts1 td1, min ts2 td2)
                                                     dropPnts = round $ (t1 - ts1)/dt
                                                 in Signal t1 t2 dt $ \pt->(sf $ pt + dropPnts)

sigContainsDur :: Duration b -> Signal a -> Bool
sigContainsDur ((td1,td2),vd) (Signal ts1 ts2 dt sf) = ts1 < td1 && ts2 > td2

sigOverlapsDur :: Duration b -> Signal a -> Bool
sigOverlapsDur ((td1,td2),vd) (Signal ts1 ts2 dt sf) = td2 > ts1 && td1<ts2 -- || td1 < ts2 && td2 >ts1

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
sigDur (Signal t1 t2 _ _) = ((t1,t2), ())

sscan :: (a->b->a) -> a -> Signal b -> Signal a
sscan f init sig@(Signal t1 t2 dt sf) = let arr2 = scanl f init $ sigToList sig
                                        in Signal t1 t2 dt $ \pt->arr2!!pt



sigSegStat :: Fold a b -> (Int, Int) -> Signal a -> b
sigSegStat (F op init c _) (n1, n2) (Signal t1 t2 dt sf) = c $! go n1 init
    where go !n !x   | n>n2 = x
                     | otherwise = go (n+1) (x `op` (sf n))

class HasTStart t where
    gettStart :: t a -> Double

instance HasTStart ((,) Double) where
    gettStart = getTStart

instance HasTStart ((,) (Double, Double)) where
    gettStart = getTStart

instance HasTStart Signal where
    gettStart (Signal t1 _ _ _) = t1

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

instance Shiftable ((Double,Double),a) where
    shift ts ((t1,t2),v) = ((t1+ts,t2+ts),v)

instance Shiftable (Signal a) where
    shift ts (Signal t1 t2 dt sf) = Signal (t1+ts) (t2+ts) dt sf 

instance Shiftable a => Shiftable [a] where
    shift ts vls = map (shift ts) vls


individually :: ListT m a -> m [a]
individually = runListT

eachOf :: Monad m => [a] -> ListT m a
eachOf xs = ListT . return $ xs

ask :: (QueryResult a, MonadIO m) => a -> StateT QState m String
ask qx = do
  x <- qResThroughSession qx
  args <- shArgs `fmap` get
  qos <- liftIO $ qReply x args
  --let str = unlines $ [s | QString s <- qos ]
  liftIO $ putStrLn $ qos
  return qos

askForLiterate :: (QueryResult a, MonadIO m) => a -> StateT QState m ()
askForLiterate qx = do
  x <- qResThroughSession qx
  args <- shArgs `fmap` get
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
       ] $ (liftIO $ putStrLn $ "<pre>=> "++str++"</pre>")
  return ()

askForLiterateTable :: (QueryResult a, MonadIO m) => a -> StateT QState m ()
askForLiterateTable qx = do
  modify (\s-> s { shArgs = "-g" : shArgs s })
  x <- qResThroughSession qx
  args <- shArgs `fmap` get
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
       ] $ (liftIO $ putStrLn $ str)
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

qs1 s = s -- [QString s]

instance QueryResult [Char] where
    qReply s _ = return $ qs1 s
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance QueryResult Int where
    qReply x _ = return .qs1$ show x
    qFilterSuccess 0 = False
    qFilterSuccess _ = True

instance QueryResult [Double] where
    qReply xs _ = return .qs1 $ show xs
    qFilterSuccess = not . null

instance QueryResult Double where
    qReply x _ = return .qs1 $ show x
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

downSample n = map (downSample' (n `div` 2))

downSample' :: (Ord a, Bounded a, Num a, Storable a) => Int -> Signal a -> Signal a
--downSample' :: Int -> Signal Double -> Signal Double
downSample' n sig@(Signal t1 t2 dt sf) =
    let x ./. y = realToFrac x / realToFrac y
        npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./. n)
        nChunks =  ceiling (npw ./. chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = SV.pack $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)./.(nChunks*2)) $ \p-> narr `SV.index` p)
           else sig
