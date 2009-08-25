{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, IncoherentInstances, DeriveDataTypeable, NoMonomorphismRestriction, BangPatterns, TypeOperators #-} 

module QueryTypes where

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
import Data.Maybe
import Math.Probably.PlotR
import Data.Unique
import TNUtils
import Data.Typeable
import Array
import Math.Probably.FoldingStats
import System.IO
import ValueIO

type Duration a = ((RealNum,RealNum),a)
type Event a = (RealNum,a)


instance PlotWithR [Signal Double] where
    getRPlotCmd sigs = 
        do ss <- mapM writeSig $ downSample 1000 sigs
           return $ RPlCmd { 
                        prePlot = map rplotcmd ss, 
                        cleanUp = return (), --mapM_ (\(df, r, t1, freq)-> removeFile df) ss,
                        plotArgs = map (\(df, r, t1, freq) -> TimeSeries ("dat"++r)) ss
                      }
        where writeSig sig@(Signal t1 t2 dt sf) = 
                  do r <- ( show . idInt . hashUnique) `fmap` newUnique
                     let datfile= "/tmp/bugplotSig"++r
                     writeSigReal datfile sig
                     --writeFile datfile . unlines $ map (\t->show $ sf t) [0..(floor $ (t2-t1)/dt)-1]
                     --print "done file!"
                     return (datfile, r, show t1, show $ 1/dt)
              rplotcmd (df, r, t1, freq) = concat ["file",r, " <- file(\"", df,"\", \"rb\")\n",
                                                   "zz<-readBin(file",r,", \"int\", 2, size=8)\n", 
                                                   "zz<-readBin(file",r,", \"int\", 2, size=1)\n", 
                                                   "ss",r," <- readBin(file",r,", \"double\", 3, size=8)\n", 
                                                   "n",r," <- (ss",r,"[2] - ss",r,"[1])/ss",r,"[3]\n",
                                                   "v",r," <- readBin(file",r,", \"double\", n",r,", size=8)\n",
                                                   "dat",r," <- ts(v",r,", start=", t1, ", frequency=", freq,")\n"]

instance Real a => PlotWithR [Event a] where
    getRPlotCmd evs = 
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = [PLPoints $ map (\(t,v)-> ( t, realToFrac v)) evs]
                      }

instance  Real a => PlotWithR [Duration a] where
    getRPlotCmd durs = 
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = map (\((t1,t2),v) -> PLLines [( t1, realToFrac v), 
                                                                 ( t2, realToFrac v)]) durs
                      }
--scatter plot
instance Tagged t => PlotWithR [t (RealNum,RealNum)] where
    getRPlotCmd ts = 
        let xys = map getTag ts in
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = [PLPoints $ map (\(t,v)-> ( t,  v)) xys]
                      }

data Hist a = forall t. Tagged t => Histogram [t a]

plot_ = getRPlotCmd

plotManySigs :: PlotWithR [a] => [a] -> [IO RPlotCmd]
plotManySigs sigs = map (\s-> getRPlotCmd [s]) sigs

class PlotWithR a => PlotMany a where
    chopByDur :: [Duration b] -> a -> [a]

instance PlotMany [Signal Double] where
    chopByDur durs sigs = map (\dur->section sigs [dur]) durs

instance (Real a) => PlotMany [Event a] where
    chopByDur durs evs = map (\dur->during evs [dur]) durs

instance (Real a) => PlotMany [Duration a] where
    chopByDur chopDurs durs = map (\dur->sectionDur1 dur durs) chopDurs

instance (PlotMany a, PlotMany b) => PlotMany (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)

plotManyBy :: PlotMany b => [Duration a] -> b -> [IO RPlotCmd]
plotManyBy durs pm = map getRPlotCmd $ chopByDur durs pm

instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs


--zipSigs :: Signal a -> Signal b -> Signal (a,b)

zipWithTime :: Signal a -> Signal (a,RealNum)
zipWithTime (Signal t1 t2 dt sf) = Signal t1 t2 dt $ \pt -> (sf pt, (realToFrac pt)*dt+t1)


sigInitialVal (Signal t1 t2 dt sf) = sf 0

foldSig :: (a->b->a) -> a -> Signal b -> a
foldSig f init sig = foldl' f init $ sigToList sig


--mapMaybe :: (a->Maybe b) -> [a] -> [b]
--mapMaybe f xs = catMaybes $ map f xs
during :: [Event a] -> [Duration b] -> [Event a]
during evs durs = concatMap (during' evs) durs
    where during' evs dur = filter (`evInDuration` dur) evs


section :: [Signal a] -> [Duration b] -> [Signal a]
section _ [] = []
section sigs (durs) = map (snd. snd) $ sectionGen sigs durs {-case find (sigOverlapsDur dur) sigs of
                            Just sig -> section1 sig dur : section sigs durs
                            Nothing -> section sigs durs-}

sectionGen :: [Signal a] -> [Duration b] -> [Duration (b,Signal a)]
sectionGen _ [] = []
sectionGen sigs (dur@(ts,v):durs) = (map f $ filter (sigOverlapsDur dur) sigs) ++ sectionGen sigs durs
    where f sig = (ts, (v,section1 sig dur)) 
                           


sectionDur1 :: Duration a -> [Duration b] -> [Duration b]
sectionDur1 ((lo,hi),_) durs = concatMap f durs
    where f ((t1,t2),v) | t2 <lo || t1 > hi = []
                        | otherwise = [((max lo t1, min hi t2), v)]


section1 :: Signal a -> ((RealNum, RealNum), t) -> Signal a
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


downSample n = map (downSample' (n `div` 2))

downSample' :: (Ord a, Bounded a, Num a) => Int -> Signal a -> Signal a
downSample' n sig@(Signal t1 t2 dt sf) =
    let npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./ n)
        nChunks =  ceiling (npw ./chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = listArray (0,(nChunks*2)-1 ) $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)./(nChunks*2)) $ \p-> narr!p)
           else sig
        {-chunk i = let arrsec = sliceU (uVpnts w) (i*chunkSize) $ min chunkSize (npw - i*chunkSize -1)
                                           in [maximumU arrsec, minimumU arrsec]
                             in UVecWave (toU narr) ((maxt w-mint w)/2 / realToFrac nChunks) (mint w) (nChunks*2)
-}

sigSegStat :: Fold a b -> (Int, Int) -> Signal a -> b
sigSegStat (F op init c _) (n1, n2) (Signal t1 t2 dt sf) = c $! go n1 init
    where go !n !x   | n>n2 = x
                     | otherwise = go (n+1) (x `op` (sf n))


x ./ y = realToFrac x / realToFrac y

class Tagged t where
    getTag :: t a-> a
    setTag :: t a-> b ->t b
    getTStart :: t a -> RealNum
    getTStop :: t a -> RealNum

instance Tagged ((,) RealNum) where
    getTag = snd
    setTag (t,_) v = (t,v)
    getTStart (t,_) = t
    getTStop (t,_) = t

instance Tagged ((,) (RealNum, RealNum)) where
    getTag (_,v) = v
    setTag ((t1,t2),_) v = ((t1,t2), v)
    getTStart ((t1,t2),_) = t1
    getTStop ((t1,t2),_) = t2

foldTagged ::  Tagged t => (a -> b -> a) -> a -> [t b] -> a
foldTagged f init col = foldl' f init $ map getTag col


instance Functor ((,) RealNum) where
    fmap f (t,v) = (t, f v)

{-instance Functor ((,) (RealNum, RealNum)) where
    fmap f ((t1,t2), v) = ((t1,t2),f v) -}
 
instance Functor Signal where
    fmap f (Signal t1 t2 dt sf) = 
        Signal t1 t2 dt $ \ix -> f (sf ix)


instance Shiftable (RealNum,a) where
    shift ts (t,v) = (t+ts, v)

instance Shiftable ((RealNum,RealNum),a) where
    shift ts ((t1,t2),v) = ((t1+ts,t2+ts),v)

instance Shiftable (Signal a) where
    shift ts (Signal t1 t2 dt sf) = Signal (t1+ts) (t2+ts) dt sf 


individually :: ListT m a -> m [a]
individually = runListT

eachOf :: Monad m => [a] -> ListT m a
eachOf xs = ListT . return $ xs

data QueryResultBox = forall a. QueryResult a => QResBox a deriving Typeable

class QueryResult a where
    qReply :: a -> IO String
    qResThroughSession :: a -> StateT Session IO a
    qResThroughSession = return 
    qFilterSuccess :: a -> Bool

instance Show a => QueryResult [Signal a] where
    qReply xs = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
instance Show a => QueryResult [Event a] where
    qReply xs = return $ show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
instance Show a => QueryResult [Duration a] where
    qReply xs =return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
instance QueryResult (IO RPlotCmd) where
    qReply ioplot = do plot <- ioplot
                       plotPlotCmd plot
                       return ""
    qFilterSuccess _ = True

instance QueryResult [IO RPlotCmd] where
    qReply ioplots = do 
      u <- (show. hashUnique) `fmap` newUnique
      let htmlFile  ="/tmp/plots"++u++".html" 
      h <- openFile (htmlFile) WriteMode
      pls <- forM ioplots $ \ioplot -> do
                                     plot <- ioplot
                                     r <- (show. hashUnique) `fmap` newUnique
                                     let fnm = "/tmp/plot"++r++".png"
                                     --putStrLn fnm
                                     hPutStrLn h $ concat ["<img src=\"file://", fnm, "\" /><p />"]
                                     return (fnm,plot)
      plotCmdToPng pls
      hClose h
      --plotPlotCmd plot
      --system $ "gnome-open file://"++ htmlFile
      return $ "file://"++ htmlFile
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance QueryResult [Char] where
    qReply = return 
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance QueryResult Int where
    qReply = return . show
    qFilterSuccess 0 = False
    qFilterSuccess _ = True

instance QueryResult RealNum where
    qReply = return . show
    qFilterSuccess 0 = False
    qFilterSuccess _ = True


    


--class (MonadState Session m, MonadIO m) => QueryM m where
--    answers :: [a] -> m a

--class MCompose m1 m2 m3 | m1 m2 -> m3 where
--    mcompose :: 
    


--instance QueryM (ListT (StateT Session IO)) Id where
 --   answers xs = ListT . return $ xs

--instance QueryM (StateT Session IO) where
--    answers = return 


--newtype AskM a = AskM { unAskM :: ListT (StateT Session IO) a }
 --   deriving (Monad, MonadIO, Functor, MonadState Session, MonadPlus)

--runAskM :: Session -> AskM a -> IO [a]
--runAskM sess (AskM lsioA) = fst `fmap` runStateT (runListT (lsioA)) sess

--answers :: [a] -> AskM a
--answers xs = AskM (ListT . return $ xs)
--answer x = AskM (ListT . return $ [x])


--old stuff 
{-askM :: Q -> AskM V
askM (Map lame q) = do
  let f v = unEvalM $ eval emptyEvalS (App lame (Const v))
  f `fmap` askM q

askM (Filter pred q) = do
  let f v = unEvalM $ eval emptyEvalS (App pred (Const v))
  vs <- askM q
  guard (isNotFalse $ f vs)
  return vs

askM (Has qep qevs) = do 
  ev <- askM qevs
  ep <- askM qep
  guard (ev `evInEpoch` ep)
  return ep


data Q = QVar String
       -- | Filter E Q
       -- | Map E Q
       | Filter E Q
       | Map E Q
       | Has Q Q
       | In Q Q
       | Around Q Q
-}