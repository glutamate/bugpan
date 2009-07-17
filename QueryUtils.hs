module QueryUtils where

import EvalM hiding (ListT)
import Eval
import Expr
import QueryTypes
import Data.List
import Data.Maybe
import Database
import Math.Probably.FoldingStats

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

later :: Double -> [Event a] -> [Event a]
later t  = map (\(te, v) ->(t+te, v))

fadeOut :: Double -> [Event a] -> [Duration a]
fadeOut t = map (\(tev,v)-> (tev, tev+t, v))

fadeIn :: Double -> [Event a] -> [Duration a]
fadeIn t = map (\(tev,v)-> (tev-t, tev, v))


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
area1 sig@(Signal t1 t2 dt sf) = (t1, t2, foldSig sumf 0 sig)
    where sumf prev next = prev+next*(realToFrac dt)

(<$$>) :: (Functor f1, Functor f2) => (a->b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

tag :: Tagged t => [t a] -> b -> [t b]
tag ts tg = map (`setTag` tg) ts

freqDuring :: [Event b] -> [Duration a] -> [Duration (a, Double)]
freqDuring evs durs = map (freqDuring' evs) durs
    where freqDuring' evs dur@(t1, t2, durtag) = 
              (t1, t2, (durtag, 
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

inout :: [Event a] -> [Event b] -> [Duration (a,b)]
inout [] _ = []
inout _ [] = []
inout ((t1,v1):ins) outs = 
    case dropWhile ((<t1) . fst) outs of
      ((t2,v2):outs') -> (t1,t2,(v1,v2)):inout ins outs'
      [] -> []

runStatsOn :: Tagged t => Fold a b -> [t a] -> [Duration b]
runStatsOn _ [] = []
runStatsOn (F op init c cmb) tgs = 
    let t1 = minimum $ map getTStart tgs
        t2 = maximum $ map getTStop tgs
        v = c . foldl' op init $ map getTag tgs 
    in [(t1,t2,v)]
