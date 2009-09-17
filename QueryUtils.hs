{-# LANGUAGE BangPatterns, NoMonomorphismRestriction, FlexibleContexts #-}

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
import Data.Ord
import TNUtils

peak :: Ord a => [Signal a] ->[Event a]
peak sigs =  map (\sig -> swap . foldSig cmp (sigInitialVal sig, 0) $ zipWithTime sig) sigs 
    where cmp (curMax, tmax) (v, t) = if v>curMax 
                                         then (v, t)
                                         else (curMax, tmax)
          swap (x,y) = (y,x)



applyOverWith :: (a->b->c) -> [Signal a] -> [Duration b] -> [Signal c]
applyOverWith f sigs durs = concatMap (aux durs) sigs --is sig within a dur? if so, apply
    where aux durs sig = map (aux1 sig) $ filter (`sigOverlapsDur` sig) durs
          aux1 sig dur = (`f` (getTag dur)) `fmap` section1 sig dur

applyOver :: [Duration (a->b)] -> [Signal a] -> [Signal b]
applyOver durs sigs = concatMap (aux durs) sigs --is sig within a dur? if so, apply
    where aux durs sig = map (aux1 sig) $ filter (`sigOverlapsDur` sig) durs
          aux1 sig dur = (getTag dur) `fmap` section1 sig dur

--applyOverDur :: [Duration (a->b)] -> [Duration a] -> [Duration b] 
(<**>) :: (Functor f, ChopByDur [f a]) => [Duration (a -> b)] -> [f a] -> [f b]
durf <**> durx = concatMap apply $ zip durf $ chopByDur durf durx 
    where apply ((_, f), durxs) = map (fmap f) durxs
          

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

infixr //

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

tagd :: Tagged t =>  Double -> [t a] -> [t Double]
tagd = tag


freqDuring :: [Duration a] -> [Event b] -> [Duration (a, RealNum)]
freqDuring durs evs = map (freqDuring' evs) durs
    where freqDuring' evs dur@((t1, t2), durtag) = 
              ((t1, t2), (durtag, 
                        (realToFrac . length $ during [dur] evs)/(t2-t1)))

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

       

spreadOut :: (Ord a, Bounded a, Num a, Fractional a) => [Signal a] -> [Signal a]
spreadOut [] = []
spreadOut (sigs) = let amp = uncurry (-) . getTag $ sigStat' (both maxF minF) (head sigs)
                       adds = map (*1.5) $ iterate (+amp) 0
                   in map (\(s,m) -> fmap (+m) s) (zip sigs adds)


intervals :: Tagged t => [t a] -> [t Double]
intervals tgs = map calcInt . zip tgs $ tail tgs
                where calcInt (t1, t2) = setTag t1 $ getTStart t2 - getTStart t1

intervalsOver :: (Tagged t, ChopByDur [t a]) =>
                 [Duration b] -> [t a] -> [t Double]
intervalsOver durs evs = concatMap (intervals) $ chopByDur durs evs

minMaxDiffF = pure (-) <*> maxF <*> minF
sigNoiseRatioF = pure (/) <*> minMaxDiffF <*> stdDevPF

dur :: a -> [Duration a]
dur x = [((minBound, maxBound), x)]

durd :: Double -> [Duration Double]
durd = dur

-- <**> :: [Duration (a->b)] -> [Duration a] -> [Duration b]

--dur (/) <**> ecVoltage <**> sigStat stdDevF ecVoltage 

--normaliseBy :: Fold a a -> [Signal a] -> [Signal a]


normaliseBy :: (Floating a) => Fold a a -> [Signal a] -> [Signal a]
normaliseBy stat sigs = ((flip (/)) <$$> sigStat stat sigs ) <**> sigs


normaliseBySD :: (Floating a) => [Signal a] -> [Signal a]
normaliseBySD = normaliseBy stdDevF


subMeanNormSD ::  (Floating a) => [Signal a] -> [Signal a]
subMeanNormSD sigs = (f <$$> sigStat stat sigs ) <**> sigs
    where stat = meanSDF
          f (mean,sd) = \x-> (x-mean)/sd

crossesUp :: [Duration Double] -> [Signal Double] -> [Event ()]
crossesUp thresDurs sigs = concatMap f $ sectionGen sigs thresDurs
    where f (_,(thresh,Signal t1 t2 dt sf)) = let npts = round $ (t2-t1)/dt
                                                  pts = [0..npts-1]
                                                  go n last hits | n >= npts = hits
                                                                 | otherwise = let this = sf n
                                                                               in if this >thresh && last < thresh
                                                                                  then go (n+1) this (n:hits)
                                                                                  else go (n+1) this (hits)
                                              in map ((\t->(t,())) .  (+t1) . (*dt) . (realToFrac)) $ reverse $ go 0 (thresh+1) []

crossesDown th sigs = crossesUp (negate <$$> th) (negate <$$> sigs)

gaussianf mean sd x = let factor = (recip $ sd*sqrt (2*pi))
                      in factor * (exp . negate $ (((x-mean)**2)/(2*sd**2)))
gaussian dt mean sd = let t1 =(-5*sd) 
                      in Signal t1 (5*sd) dt $ \p-> gaussianf mean sd ((realToFrac p)*dt+t1)

convolveWithin :: Num a => [Duration b] -> Signal a -> [Event a] -> [Signal a]
convolveWithin [] _ _ = []
convolveWithin (dur@((td1, td2), v):durs) irf@(Signal t1 t2 dt sf) evs' =
   let evs = during [dur] evs' 
       sigs = map f evs
       f (t,x) =(*x) `fmap` shift t irf
       nullSig = Signal td1 td2 dt $ const 0
       addSigs (Signal ts1 ts2 _ ssf) (Signal ts1' ts2' _ ssf') = Signal ts1 ts2 dt $ \p->
                                                                  let t = (realToFrac p)*dt+ts1
                                                                  in ssf p + (if t>ts1' && t<ts2' 
                                                                                 then ssf' . round $ (t-ts1')/dt
                                                                                 else 0)
       sig = foldl' addSigs nullSig sigs
   in sig : convolveWithin (durs) irf evs'

--[Duration a] -> [Duration b] -> [Duration (a,b)]
zipD :: (Functor f, ChopByDur [f b]) => [Duration a] -> [f b] -> [f (a, b)]
zipD durx dury = ((,) <$$> durx) <**> dury

{-countWithin :: [Duration a] -> [Event b] -> [Duration Double]
countWithin (dur:durs) evs = concatMap f durs
    where f dur = (realToFrac . length $ evs `during` [dur] ) `tag` [dur]
-}

crossCorrelateOver :: [Duration a] -> [Event a] -> [Event b] -> [Event Double]
crossCorrelateOver dur e1 e2 = concatMap f $ zip (chopByDur dur e1) (chopByDur dur e2)
    where f (evs1, evs2) = concatMap (g evs2) evs1
          g evs2 (t0,_) = map (\(t2,_)->(t2,t2-t0)) evs2

limitSigs :: Double -> Double -> [Signal a] -> [Signal a]
limitSigs lo hi sigs = map (limitSig (min lo hi) (max lo hi)) sigs

limitSigs' :: Double -> Double -> [Signal a] -> [Signal a]
limitSigs' lo hi sigs = catMaybes $ map (limitSig' (min lo hi) (max lo hi)) sigs

limitSig' lo hi (Signal t1 t2 dt sf) | t1 > lo || t2< hi = Nothing
                                     | otherwise = let t1' = max t1 lo
                                                       t2' = min hi t2
                                                       pshift = round $ (t1' - t1)/dt
                                                   in Just $ Signal t1' t2' dt $ \p-> sf $ p+pshift


averageSigs :: Floating a => [Signal a] -> [Signal a]
averageSigs sigs = let (mu, sem) = runStat meanSEMF sigs
                   in [mu,mu+sem, mu-sem]


tagMany :: Tagged t => [a] -> [t b] -> [t a]
tagMany [] _ = []
tagMany _ [] = []
tagMany (x:xs) (t:tgs) = setTag t x : tagMany xs tgs 

cycleLabel :: [Int] -> [Duration a] -> [Duration Int]
cycleLabel xs dur = tagMany (cycle xs) dur

groupBy :: (Functor f, ChopByDur [f b], Eq a) => [Duration a] -> [f b] -> [(a, [f b])]
groupBy durs eps = let uncatted = zip (map getTag durs) $ chopByDur durs eps
                       differentAs = nub $ map fst uncatted
                       --catThem a = (a, concat $ lookupMany a uncatted)
                       --catted = map catThem uncatted
                   in uncatted

--chiSquare :: [[Duration a]] -> 