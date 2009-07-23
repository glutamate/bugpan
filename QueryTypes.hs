{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, ExistentialQuantification, IncoherentInstances #-} 

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

type Duration a = (Double,Double,a)
type Event a = (Double,a)

data Signal a = Signal Double Double Double (Int -> a) 

instance Show a =>  Show (Signal a) where
    show sig@(Signal t1 t2 dt sf) = "{"++show t1++": "++(show . take 5 $ sigToList sig)++"... :"++show t2++"}"
instance Num a => PlotWithR [Signal a] where
    getRPlotCmd sigs = 
        do ss <- mapM writeSig sigs
           return $ RPlCmd { 
                        prePlot = map (\(df, r, t1, freq) -> concat ["dat",r, " <- ts(scan(\"", df, "\"), start=", t1, ", frequency=", freq,")"]) ss, 
                        cleanUp = mapM_ (\(df, r, t1, freq)-> removeFile df) ss,
                        plotArgs = map (\(df, r, t1, freq) -> TimeSeries ("dat"++r)) ss
                      }
        where writeSig sig@(Signal t1 t2 dt sf) = 
                  do r <- ( show . idInt . hashUnique) `fmap` newUnique
                     let datfile= "/tmp/bugplot"++r
                     writeFile datfile . unlines $ map (\t->show $ sf t) [0..round $ (t2-t1)/dt]
                     return (datfile, r, show t1, show $ 1/dt)

instance Real a => PlotWithR [Event a] where
    getRPlotCmd evs = 
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = [PLPoints $ map (\(t,v)-> (t, realToFrac v)) evs]
                      }

instance PlotWithR [Duration Double] where
    getRPlotCmd durs = 
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = map (\(t1,t2,v) -> PLLines [(t1, v), (t2, v)]) durs
                      }
--scatter plot
instance Tagged t => PlotWithR [t (Double,Double)] where
    getRPlotCmd ts = 
        let xys = map getTag ts in
        do return $ RPlCmd { 
                        prePlot = [],
                        cleanUp = return (),
                        plotArgs = [PLPoints xys]
                      }

data Hist a = forall t. Tagged t => Histogram [t a]

instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

sigPnts :: Signal a -> Int
sigPnts (Signal t1 t2 dt sf) = round $ (t2-t1)/dt

--zipSigs :: Signal a -> Signal b -> Signal (a,b)

zipWithTime :: Signal a -> Signal (a,Double)
zipWithTime (Signal t1 t2 dt sf) = Signal t1 t2 dt $ \pt -> (sf pt, (realToFrac pt)*dt+t1)

sigToList :: Signal a -> [a]
sigToList sig@(Signal t1 t2 dt sf) = map sf [0..sigPnts sig-1]

sigInitialVal (Signal t1 t2 dt sf) = sf 0

foldSig :: (a->b->a) -> a -> Signal b -> a
foldSig f init sig = foldl' f init $ sigToList sig


--mapMaybe :: (a->Maybe b) -> [a] -> [b]
--mapMaybe f xs = catMaybes $ map f xs
section1 (Signal ts1 ts2 dt sf) (td1,td2,vd) = let (t1, t2)= (max ts1 td1, min ts2 td2)
                                                   dropPnts = round $ (t1 - ts1)/dt
                                               in Signal t1 t2 dt $ \pt->(sf $ pt + dropPnts)

sigContainsDur :: Duration b -> Signal a -> Bool
sigContainsDur (td1,td2,vd) (Signal ts1 ts2 dt sf) = ts1 < td1 && ts2 > td2

sigOverlapsDur :: Duration b -> Signal a -> Bool
sigOverlapsDur (td1,td2,vd) (Signal ts1 ts2 dt sf) = td2 > ts1 && td1<ts2 -- || td1 < ts2 && td2 >ts1

type List a = [a]
type Id a = a

vToEvent v = (evTime v, evTag v)
vToDuration v = let (t1, t2) = epTs v in (t1, t2, epTag v)

evInDuration (t,_) (t1,t2, _) = t<t2 && t>t1


showDur (t1,t2,v) = show t1 ++ ".."++show t2++": "++show v
showEvt (t,v) = "@"++show t++": "++show v

sigDur :: Signal a -> Duration ()
sigDur (Signal t1 t2 _ _) = (t1,t2, ())

sscan :: (a->b->a) -> a -> Signal b -> Signal a
sscan f init sig@(Signal t1 t2 dt sf) = let arr2 = scanl f init $ sigToList sig
                                        in Signal t1 t2 dt $ \pt->arr2!!pt


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

instance Tagged ((,,) Double Double) where
    getTag (_,_,v) = v
    setTag (t1,t2,_) v = (t1,t2, v)
    getTStart (t1,t2,_) = t1
    getTStop (t1,t2,_) = t2

foldTagged ::  Tagged t => (a -> b -> a) -> a -> [t b] -> a
foldTagged f init col = foldl' f init $ map getTag col


instance Functor ((,) Double) where
    fmap f (t,v) = (t, f v)

instance Functor ((,,) Double Double) where
    fmap f (t1,t2, v) = (t1,t2,f v)
 
instance Functor Signal where
    fmap f (Signal t1 t2 dt sf) = 
        Signal t1 t2 dt $ \ix -> f (sf ix)


instance Shiftable (Double,a) where
    shift ts (t,v) = (t+ts, v)

instance Shiftable (Double,Double,a) where
    shift ts (t1,t2,v) = (t1+ts,t2+ts,v)

instance Shiftable (Signal a) where
    shift ts (Signal t1 t2 dt sf) = Signal (t1+ts) (t2+ts) dt sf 


class Reify a where
    reify :: V-> Maybe a
    pack :: a->V

instance Reify V where 
    reify = Just
    pack = id
instance Reify Double where 
    reify = vToDbl
    pack = NumV . NReal
instance Reify Int where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just i
    reify _ = Nothing
    pack = NumV . NInt
instance Reify Integer where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just $ toInteger i
    reify _ = Nothing
    pack = NumV . NInt . fromInteger
instance (Reify a, Reify b) => Reify (a,b) where
    reify (PairV a b) = liftM2 (,) (reify a) (reify b)
    reify _ = Nothing
    pack (x,y) = PairV (pack x) (pack y)
instance (Reify a, Reify b, Reify c) => Reify (a,b,c) where
    reify (PairV (PairV a b) c) = liftM3 (,,) (reify a) (reify b) (reify c)
    reify _ = Nothing
    pack (x,y,z) = PairV (PairV (pack x) (pack y)) (pack z)
instance (Reify a) => Reify [a] where
    reify (ListV xs) = let rmxs = map reify xs in
                       if all isJust rmxs
                          then Just $ map fromJust rmxs
                          else Nothing
    reify _ = Nothing
    pack xs = ListV $ map pack xs
instance Reify NumVl where
    reify (NumV n) = Just n
    reify _ = Nothing
    pack = NumV
instance Reify () where
    reify Unit = Just ()
    reify _ = Nothing
    pack () = Unit
instance Reify a => Reify (Signal a) where
    reify (SigV t1 t2 dt sf) = Just $ Signal t1 t2 dt $ \ix-> unsafeReify (sf ix)
    pack (Signal t1 t2 dt sf) = SigV t1 t2 dt $ \ix->pack (sf ix)

unsafeReify :: Reify a => V -> a
unsafeReify = fromJust . reify

individually :: ListT m a -> m [a]
individually = runListT

eachOf :: Monad m => [a] -> ListT m a
eachOf xs = ListT . return $ xs


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