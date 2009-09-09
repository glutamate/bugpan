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
--import Math.Probably.PlotR
import Data.Unique
import TNUtils
import Data.Typeable
import Math.Probably.FoldingStats
import System.IO
import ValueIO

type Duration a = ((Double,Double),a)
type Event a = (Double,a)

data QState = QState { qsSess:: Session,
                       lastTStart:: Double,
                       lastTStop :: Double,
                       realTime :: Bool}

getSession = qsSess `fmap` get

simulatedTime = do qs <- get
                   put $ qs { realTime = False }


{-
--scatter plot
instance Tagged t => PlotWithR [t (Double,Double)] where
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
-}
--class PlotWithR a => PlotMany a where


--zipSigs :: Signal a -> Signal b -> Signal (a,b)

zipWithTime :: Signal a -> Signal (a,Double)
zipWithTime (Signal t1 t2 dt sf) = Signal t1 t2 dt $ \pt -> (sf pt, (realToFrac pt)*dt+t1)


sigInitialVal (Signal t1 t2 dt sf) = sf 0

foldSig :: (a->b->a) -> a -> Signal b -> a
foldSig f init sig = foldl' f init $ sigToList sig


--mapMaybe :: (a->Maybe b) -> [a] -> [b]
--mapMaybe f xs = catMaybes $ map f xs
during :: [Duration b] -> [Event a] -> [Event a]
during durs evs = concatMap (during' evs) durs
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

ask :: QueryResult a => a -> StateT QState IO ()
ask qx = do
  x <- qResThroughSession qx
  str <- liftIO $ qReply x []
  liftIO $ putStrLn str


data QueryResultBox = forall a. QueryResult a => QResBox a deriving Typeable

class QueryResult a where
    qReply :: a -> [String] -> IO String
    qResThroughSession :: a -> StateT QState IO a
    qResThroughSession = return 
    qFilterSuccess :: a -> Bool

instance Show a => QueryResult [Signal a] where
    qReply xs _ = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
instance Show a => QueryResult [Event a] where
    qReply xs _ = return $ show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
instance Show a => QueryResult [Duration a] where
    qReply xs _ = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True
{-instance QueryResult (IO RPlotCmd) where
    qReply ioplot = do plot <- ioplot
                       plotPlotCmd plot
                       return ""
    qFilterSuccess _ = True

instance QueryResult [IO RPlotCmd] where
    qReply ioplots = do 
      u <- (show. hashUnique) `fmap` newUnique
      let htmlFile  ="/var/bugpan/www/plots"++u++".html" 
      h <- openFile (htmlFile) WriteMode
      pls <- forM ioplots $ \ioplot -> do
                                     plot <- ioplot
                                     r <- (show. hashUnique) `fmap` newUnique
                                     let fnm = r++".png"
                                     --putStrLn fnm
                                     hPutStrLn h $ concat ["<img src=\"", fnm, "\" /><p />"]
                                     return ("/var/bugpan/www/"++fnm,plot)
      plotCmdToPng pls
      hClose h
      --plotPlotCmd plot
      --system $ "gnome-open file://"++ htmlFile
      return $ "file://"++ htmlFile
    qFilterSuccess [] = False
    qFilterSuccess _ = True

-}
instance QueryResult [Char] where
    qReply s _ = return s 
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance QueryResult Int where
    qReply x _ = return $  show x
    qFilterSuccess 0 = False
    qFilterSuccess _ = True

instance QueryResult Double where
    qReply x _ = return $ show x
    qFilterSuccess 0 = False
    qFilterSuccess _ = True


class ChopByDur a where
    chopByDur :: [Duration b] -> a -> [a]

instance ChopByDur [Signal a] where
    chopByDur durs sigs = map (\dur->section sigs [dur]) durs

instance ChopByDur [Event a] where
    chopByDur durs evs = map (\dur->during [dur] evs) durs

instance ChopByDur [Duration a] where
    chopByDur chopDurs durs = map (\dur->sectionDur1 dur durs) chopDurs


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