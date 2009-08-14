{-# LANGUAGE BangPatterns #-}

module QueryUtils where

import EvalM hiding (ListT)
import Eval
import Expr
import QueryTypes
import Data.List
import Data.Maybe
import Database
import Math.Probably.FoldingStats
import Control.Applicative hiding ((<**>))
import Numbers
import Array
import Data.Ord

peak :: Ord a => [Signal a] ->[Event a]
peak sigs =  map (\sig -> swap . foldSig cmp (sigInitialVal sig, 0) $ zipWithTime sig) sigs 
    where cmp (curMax, tmax) (v, t) = if v>curMax 
                                         then (v, t)
                                         else (curMax, tmax)
          swap (x,y) = (y,x)

section :: [Signal a] -> [Duration b] -> [Signal a]
section _ [] = []
section sigs (dur:durs) = case find (sigContainsDur dur) sigs of
                            Just sig -> section1 sig dur : section sigs durs
                            Nothing -> section sigs durs


applyOverWith :: (a->b->c) -> [Signal a] -> [Duration b] -> [Signal c]
applyOverWith f sigs durs = concatMap (aux durs) sigs --is sig within a dur? if so, apply
    where aux durs sig = map (aux1 sig) $ filter (`sigOverlapsDur` sig) durs
          aux1 sig dur = (`f` (getTag dur)) `fmap` section1 sig dur

applyOver :: [Duration (a->b)] -> [Signal a] -> [Signal b]
applyOver durs sigs = concatMap (aux durs) sigs --is sig within a dur? if so, apply
    where aux durs sig = map (aux1 sig) $ filter (`sigOverlapsDur` sig) durs
          aux1 sig dur = (getTag dur) `fmap` section1 sig dur

--with :: a -> (a-> b) -> b
--with x f = f x

later :: RealNum -> [Event a] -> [Event a]
later t  = map (\(te, v) ->(t+te, v))

fadeOut :: RealNum -> [Event a] -> [Duration a]
fadeOut t = map (\(tev,v)-> ((tev, tev+t), v))

fadeIn :: RealNum -> [Event a] -> [Duration a]
fadeIn t = map (\(tev,v)-> ((tev-t, tev), v))


filterTag :: Tagged t => (a->Bool) -> [t a] -> [t a]
filterTag p = filter (p . getTag)

(//) :: Tagged t => (a->Bool) -> [t a] -> [t a]
(//) = filterTag

(&) :: (Tagged t1, Tagged t2) => [t1 a] -> [t2 b] -> [t2 b]
[] & _ = []
_ & ys = ys

-- (=58) session 
-- x = [] & []

area :: Fractional a => [Signal a] -> [Duration a]
area sigs = map area1 sigs

area1 :: Fractional a => Signal a -> Duration a
area1 sig@(Signal t1 t2 dt sf) = ((t1, t2), foldSig sumf 0 sig)
    where sumf prev next = prev+next*(realToFrac dt)

(<$$>) :: (Functor f1, Functor f2) => (a->b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

tag :: Tagged t =>  b -> [t a] -> [t b]
tag tg = map (`setTag` tg)

freqDuring :: [Event b] -> [Duration a] -> [Duration (a, RealNum)]
freqDuring evs durs = map (freqDuring' evs) durs
    where freqDuring' evs dur@((t1, t2), durtag) = 
              ((t1, t2), (durtag, 
                        (realToFrac . length $ evs `during` [dur])/(t2-t1)))

during :: [Event a] -> [Duration b] -> [Event a]
during evs durs = concatMap (during' evs) durs
    where during' evs dur = filter (`evInDuration` dur) evs

around :: [Event b] -> [Signal a] -> [Signal a]
around evs sigs = catMaybes $ map (around' sigs) evs
    where around' sigs ev@(t,_) = 
              case filter ((ev `evInDuration`) . sigDur) sigs of
                (sig:_) -> Just $ shift (-t) sig
                [] -> Nothing

align :: [Event b] -> [Signal a] -> [Signal a]
align evs sigs = map align' sigs
    where align' sig@(Signal t1 t2 dt sf) = let (ts,_) = minimumBy (comparing ((distFrom t1) . fst)) evs
                                            in shift (negate ts) sig
          distFrom x y = abs(x-y)

inout :: [Event a] -> [Event b] -> [Duration (a,b)]
inout [] _ = []
inout _ [] = []
inout ((t1,v1):ins) outs = 
    case dropWhile ((<t1) . fst) outs of
      ((t2,v2):outs') -> ((t1,t2),(v1,v2)):inout ins outs'
      [] -> []

runStatsOn :: Tagged t => Fold a b -> [t a] -> [Duration b]
runStatsOn _ [] = []
runStatsOn (F op init c cmb) tgs = 
    let t1 = minimum $ map getTStart tgs
        t2 = maximum $ map getTStop tgs
        v = c . foldl' op init $ map getTag tgs 
    in [((t1,t2),v)]

sigStat :: Fold a b -> [Signal a] -> [Duration b]
sigStat f sigs = map (sigStat' f) sigs
 
sigStat' :: Fold a b -> Signal a -> Duration b
sigStat' (F op init c cmb) sig@(Signal t1 t2 dt sf) = 
    let --v = c . foldl' op init $ sigToList sig
        
        --go 0 x = c x
        --go n x = go (n-1) (x `op` sf n)
        v = c $! go npts init
    in ((t1,t2),v)
       where npts = round $ (t2-t1)/dt
             go 0 x = x
             go !n !x = go (n-1) (x `op` sf (npts-n))

        

intervals :: Tagged t => [t a] -> [t (a,RealNum)]
intervals tgs = map calcInt . zip tgs $ tail tgs
                where calcInt (t1, t2) = setTag t1 $ (getTag t1, getTStart t2 - getTStart t1)

minMaxDiffF = pure (-) <*> maxF <*> minF
sigNoiseRatioF = pure (/) <*> minMaxDiffF <*> stdDevPF

dur :: a -> [Duration a]
dur x = [((minBound, maxBound), x)]

-- <**> :: [Duration (a->b)] -> [Duration a] -> [Duration b]

