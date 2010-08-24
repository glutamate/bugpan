{-# LANGUAGE BangPatterns, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, GADTs #-}

module QueryUtils where

import EvalM hiding (ListT)
--import Eval
--import Expr
import QueryTypes
import Data.List hiding (groupBy)
import Data.Maybe
import Database
import Math.Probably.FoldingStats
import Control.Applicative hiding ((<**>))
import Numbers
import Data.Ord
import TNUtils
import Math.Probably.Sampler
import Math.Probably.GlobalRandoms
import Control.Monad
import PlotGnuplot
import NewSignal
import Foreign.Storable
import qualified Data.StorableVector as SV



peak :: (Storable a, Ord a) => [Signal a] ->[Event a]
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


hold :: [Event a] -> [Duration a]
hold [] = []
hold ((t,v):[]) = [((t,maxBound), v)]
hold ((t1,v):rest@((t2,_):more)) = ((t1,t2),v): hold rest

filterTag :: Tagged t => (a->Bool) -> [t a] -> [t a]
filterTag p = filter (p . getTag)

infixr //

infixl 3 &

(//) :: Tagged t => (a->Bool) -> [t a] -> [t a]
(//) = filterTag

(&) :: (Tagged t1, Tagged t2) => [t1 a] -> [t2 b] -> [t2 b]
[] & _ = []
_ & ys = ys

notD :: [a] -> [()]
notD [] = [()]
notD _ = []

-- (=58) session 
-- x = [] & []

area :: Fractional a => [Signal a] -> [Duration a]
area sigs = map area1 sigs

area1 :: Fractional a => Signal a -> Duration a
area1 sig@(Signal t1 t2 dt _ _) = ((t1, t2), foldSig sumf 0 sig)
    where sumf prev next = prev+next*(realToFrac dt)

(<$$>) :: (Functor f1, Functor f2) => (a->b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

tag :: Tagged t =>  b -> [t a] -> [t b]
tag tg = map (`setTag` tg)

tagd :: Tagged t =>  Double -> [t a] -> [t Double]
tagd = tag

rebase :: (ChopByDur [t], Shiftable [t]) => [Duration b] -> [t] -> [t]
rebase durs evs = concatMap f durs
    where f dur@((t1,_),_) = shift (negate t1) $ concat $ chopByDur [dur] evs

freqDuring :: [Duration a] -> [Event b] -> [Duration Double]
freqDuring durs evs = map (freqDuring' evs) durs
    where freqDuring' evs dur@((t1, t2), durtag) = 
              ((t1, t2), (realToFrac . length $ during [dur] evs)/(t2-t1))

totalDuration :: [Duration a] -> Double
totalDuration = sum . map ((uncurry $flip (-)) . fst)

dursToOne :: [Duration a] -> [Duration ()]
dursToOne durs = [((foldl1 min (map (fst. fst) durs),foldl1 max (map (snd. fst) durs)),()) ]

frequency :: [Event b] -> [Duration a] -> [Duration Double]
frequency evs durs =
    tagd ((realToFrac . sumTags $ countDuring durs evs )  / (totalDuration durs)) $ dursToOne durs

countDuring :: [Duration a] -> [Event b] -> [Duration Int]
countDuring durs evs = map (freqDuring' evs) durs
    where freqDuring' evs dur@((t1, t2), durtag) = 
              ((t1, t2), (length $ during [dur] evs))

realcountDuring :: [Duration a] -> [Event b] -> [Duration Double]
realcountDuring x y = realToFrac <$$> countDuring x y

centreOfMass :: [Signal Double] -> [Event ()]
centreOfMass = map f 
    where f s@(Signal t1 t2 dt _ _) = let vls =sigToList s
                                          tms = sigTimePoints s
                                      in ((sum $ zipWith (*) vls tms) / sum vls, ())

centreOfMassScatter :: [(Double, Double)] -> Double
centreOfMassScatter pts = let vls =map snd pts
                              tms = map fst pts
                              denom = (sum $ zipWith (*) vls tms) 
                          in (denom / sum vls)

--square x = x*x




                                   
upsample n = map (upsample' n)
upsample' :: Int -> Signal Double -> Signal Double
upsample' n s@(Signal t1 t2 dt arr Eq) = let narr = svInterpLin n arr
                                         in Signal (t1) (t2) (dt/(realToFrac n)) (narr) Eq
upsample' n s = upsample' n $ forceSigEq s
--    let newdt = (dt/(realToFrac n))
--    in Signal t1 t2 newdt $ \p -> interp s ((realToFrac p)*newdt+t1) 

{-downsample n = map (downsample' n)
downsample' n s@(Signal t1 t2 dt sf) = 
    let newdt = (dt*(realToFrac n))
    in Signal t1 t2 newdt $ \p -> interp s ((realToFrac p)*newdt+t1) 
-}


unjitter = map unjitterSig

triSig = [listToSig 0.1 (0.0) [0..9]]

htrisig = head triSig

around :: [Event b] -> [Signal a] -> [Signal a]
around evs sigs = catMaybes $ map (around' sigs) evs
    where around' sigs ev@(t,_) = 
              case filter ((ev `evInDuration`) . sigDur) sigs of
                (sig:_) -> Just $ shift (-t) sig
                [] -> Nothing

align :: [Event b] -> [Signal a] -> [Signal a]
align evs sigs = map align' sigs
    where align' sig@(Signal t1 t2 dt sf _) = 
              let (ts,_) = minimumBy (comparing ((distFrom t1) . fst)) evs
              in shift (negate ts) sig
          distFrom x y = abs(x-y)



alignBy :: ([Signal a] -> [Event b]) -> [Signal a] -> [Signal a]
alignBy f = concatMap g
    where g sig = align (f [sig]) [sig]

sigStart :: [Signal a] -> [Event ()]
sigStart = map f
    where f sig@(Signal t1 t2 dt sf _) = (t1,())

sigStop :: [Signal a] -> [Event ()]
sigStop = map f
    where f sig@(Signal t1 t2 dt sf _) = (t2,())

durStart :: [Duration a] -> [Event a]
durStart = map f
    where f ((t1,t2),v) = (t1,v)

durStop :: [Duration a] -> [Event a]
durStop = map f
    where f ((t1,t2),v) = (t2,v)


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
sigStat' (F op init c cmb) sig@(Signal t1 t2 dt sf _) = 
    let --v = c . foldl' op init $ sigToList sig
        
        --go 0 x = c x
        --go n x = go (n-1) (x `op` sf n)
        v = c $! foldSig op init sig 
    in ((t1,t2),v)

baseline :: Double -> Double -> [Signal Double] -> [Signal Double]
baseline tb1 tb2 = map f where
  f s@(Signal t1 t2 _ _ _) = 
     let bsig = limitSig (t1+tb1) (t1+tb2) s
         bval = snd $ sigStat' meanF bsig
     in fmap (subtract bval) s

x === ys = (x `isPrefixOf`)//ys

catevents :: [Event a] -> [Event a] -> [Event a]
catevents e1s = sortBy (comparing fst) . (e1s++)
       

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

fromto :: Double -> Double -> [Duration ()]
fromto t1 t2 = [((t1, t2),())]

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

crossesUp :: Ord a => [Duration a] -> [Signal a] -> [Event ()]
crossesUp thresDurs sigs = concatMap f $ sectionGen sigs thresDurs
    where f (_,(thresh,s@(Signal t1 t2 dt _ _))) = map (flip (,) ()) $ crossSigUp thresh s
{-              let npts = sigPnts s
                  --pts = [0..npts-1]                
                  go n last hits | n <npts -1 = let this = readSigPt s n
                                                 in if this >thresh && last < thresh
                                                      then go (n+1) this (n:hits)
                                                      else go (n+1) this (hits)
                                 | otherwise = hits
              in map ((\t->(t,())) .  (+t1) . (*dt) . realToFrac) $ reverse $ go  0 (thresh+1) []
-}

crossesDown th sigs = crossesUp (negate <$$> th) (negate <$$> sigs)

crossesUpOrDown :: (Num a, Ord a) => [Duration a] -> [Signal a] -> [Event ()]
crossesUpOrDown thresDurs sigs = concatMap f $ sectionGen sigs thresDurs
    where f (_,(thresh,s@(Signal t1 t2 dt _ _))) = if thresh >0 
                                                      then (>thresh) ?? s
                                                      else (<thresh) ?? s
{-              let npts = sigPnts s
                  --pts = [0..npts-1]                
                  go n last hits | n <npts -1 = let this = readSigPt s n
                                                 in if this >thresh && last < thresh
                                                      then go (n+1) this (n:hits)
                                                      else go (n+1) this (hits)
                                 | otherwise = hits
              in map ((\t->(t,())) .  (+t1) . (*dt) . realToFrac) $ reverse $ go  0 (thresh+1) []
-}


gaussianf :: Double -> Double -> Double ->  Double
gaussianf mean sd x = let factor = (recip $ sd*sqrt (2*pi))
                      in factor * (exp . negate $ (((x-mean)**2)/(2*sd**2)))
gaussian :: Double -> Double -> Double -> Signal Double
gaussian dt mean sd = let t1 =(-5*sd) 
                          sig = Signal t1 (5*sd) dt arr Eq
                          arr = SV.pack $ map (gaussianf mean sd) (sigTimePoints sig)
                      in sig 

tagValues :: (Eq a, Tagged t) => [t a] -> [a]
tagValues = nub . map getTag

tagRange :: (Ord a, Tagged t, Fractional a) => [t a] -> (a,a)
tagRange tgs = let vls = map getTag tgs in 
               case vls of 
                 [] -> (1/0, 1/0)
                 _ -> (foldr1 min vls, foldr1 max vls) 


convolveWithin :: (Storable a, Num a) => [Duration b] -> Signal a -> [Event a] -> [Signal a]
convolveWithin [] _ _ = []
convolveWithin (dur@((td1, td2), v):durs) irf@(Signal t1 t2 dt sf Eq) evs' =
   let evs = sortBy (comparing fst) $ during [dur] evs' 
       npts = round $ (td2-td1)/dt
       g (pt, currentEs, unconsummedEs) = 
           let t = pt*dt+td1
               (newEs, notyetEs) = span ((<(t-t1)) . fst) unconsummedEs --t1 negative
               es = (dropWhile (((<(t+t2)) . fst)) currentEs)++newEs
               amp (tev,_) = readSig irf (t-tev)
            in Just (sum $ map amp es, (pt+1, es, notyetEs))
       arr = fst $ SV.unfoldrN npts g (0, [], evs)
   in Signal td1 td2 dt arr Eq : convolveWithin (durs) irf evs' 
{-       sigs = map f evs
       f (t,x) =(*x) `fmap` shift t irf
       nullSig = Signal td1 td2 dt (SV.replicate (sigPnts nullSig) 0) Eq
       addSigs :: (Storable a, Num a) => Signal a -> Signal a -> Signal a
       addSigs s@(Signal ts1 ts2 _ ssf Eq) (Signal ts1' ts2' _ ssf' Eq) =            
           Signal ts1 ts2 dt (SV.sample (sigPnts s) $ \p->
                     let t = (realToFrac p)*dt+ts1
                     in ssf `SV.index` p + (if t>ts1' && t<ts2' 
                                               then ssf' `SV.index` (round $ (t-ts1')/dt)
                                               else 0)) Eq
       sig = foldl' addSigs nullSig sigs
   in sig : convolveWithin (durs) irf evs' -}
convolveWithin durs irf evs' = convolveWithin durs (forceSigEq irf) evs'

--[Duration a] -> [Duration b] -> [Duration (a,b)]
zipD :: (Functor f, ChopByDur [f b]) => [Duration a] -> [f b] -> [f (a, b)]
zipD durx dury = ((,) <$$> durx) <**> dury

{-countWithin :: [Duration a] -> [Event b] -> [Duration Double]
countWithin (dur:durs) evs = concatMap f durs
    where f dur = (realToFrac . length $ evs `during` [dur] ) `tag` [dur]
-}

crossCorrelateOver :: [Duration a] -> [Event b] -> [Event c] -> [Event Double]
crossCorrelateOver dur e1 e2 = concatMap f $ zip (chopByDur dur e1) (chopByDur dur e2)
    where f (evs1, evs2) = concatMap (g evs2) evs1
          g evs2 (t0,_) = map (\(t2,_)->(t2,t2-t0)) evs2


jitterDbls :: Double -> [Double] -> [Double]
jitterDbls rng xs = 
    let sam = forM xs $ \x-> uniform (x-rng/2) (x+rng/2)               
    in head $ sampleN 1 sam

jitterxs :: Double -> [(Double, a)] -> [(Double, a)]
jitterxs rng es = let xs = map fst es
                      ys = map snd es
                  in zip (jitterDbls rng xs) ys

jitterys :: Double -> [(a, Double)] -> [(a, Double)]
jitterys rng es = let xs = map fst es
                      ys = map snd es
                  in zip xs (jitterDbls rng ys)

crossCorrelateOverControl :: [Duration a] -> [Event b] -> [Event c] -> [Event Double]
crossCorrelateOverControl dur e1 e2 = 
    let oneSim = do
          durOver <- oneOf dur
          let tStart = fst . fst $ durOver
          let durLength = realToFrac $ (snd $ fst durOver) - (fst $ fst durOver)
          let e1s = length $ during [durOver] e1
          let e2s = length $ during [durOver] e2
          if e1s == 0 || e2s == 0 
             then return [] -- traceM "one has zero events!" >> return []
             else do
               --traceM "both has more than zero events!"
               e1sim <- poissonMany (realToFrac e1s/durLength) durLength
               e2sim <- poissonMany (realToFrac e2s/durLength) durLength          
               return $ crossCorrelateOver [durOver] (shift tStart $ lstToEvs e1sim) (shift tStart $ lstToEvs e2sim)  
    in concat (sampleN 1000  oneSim)               
          
crossCorrelateControlled ::  [Duration a] -> Double -> [Event b] -> [Event c] -> [Signal Double]
crossCorrelateControlled dur bz e1 e2 = 
    let ctrl =crossCorrelateOverControl dur e1 e2
        s1 = normSigToArea (histSigBZ bz (crossCorrelateOver dur e1 e2))
        s2 = normSigToArea $ histSigBZ bz (ctrl)
    in [s1-s2]


testSampler rnds = concat $ take 10 $ runSampler rnds $ do 
  u <- unitSample
  if u >0.5
     then return []
     else return [()]

lstToEvs :: [Double] -> [Event ()]
lstToEvs occs = zip occs $ repeat ()

      
limitSigs :: Double -> Double -> [Signal a] -> [Signal a]
limitSigs lo hi sigs = map (limitSig (min lo hi) (max lo hi)) sigs

limitSigs' :: Double -> Double -> [Signal a] -> [Signal a]
limitSigs' lo hi sigs = catMaybes $ map (limitSig' (min lo hi) (max lo hi)) sigs


averageSigs :: (Floating a, Storable a) => [Signal a] -> [Signal a]
averageSigs sigs = let (mu, sem) = runStat meanSEMF sigs
                   in [mu,mu+sem, mu-sem]

varianceSigs :: (Floating a, Storable a) => [Signal a] -> [Signal a]
varianceSigs sigs = [runStat varF sigs]

tagMany :: Tagged t => [a] -> [t b] -> [t a]
tagMany [] _ = []
tagMany _ [] = []
tagMany (x:xs) (t:tgs) = setTag t x : tagMany xs tgs 

cycleLabel :: [Int] -> [Duration a] -> [Duration Int]
cycleLabel xs durs = tagMany (cycle xs) durs

whollyCycleLabel :: [Int] -> [Duration a] -> [Duration Int]
whollyCycleLabel xs durs = let nxs = length xs 
                           in if length durs >= nxs
                                 then tagMany xs (take nxs durs) ++ whollyCycleLabel xs (drop nxs durs)
                                 else []

groupBy :: (Functor f, ChopByDur [f b], Eq a) => [Duration a] -> [f b] -> [(a, [f b])]
groupBy durs eps = let uncatted = zip (map getTag durs) $ chopped
                       chopped = chopByDur durs eps
                       differentAs = nub $ map getTag durs
                       catThem a = (a, concat $ lookupMany a uncatted)
                       catted = map catThem differentAs
                   in catted

groupStats :: Tagged t => [(a, [t b])] -> Fold b c -> [(a, c)]
groupStats gp stat = map runS gp
    where runS (x, tgs) = (x, runStat stat $map getTag tgs)

tagTime :: Tagged t => [t a] -> [t Double]
tagTime tgs = map (\tgd -> setTag tgd $ getTStart tgd) tgs

dropSecs :: Double -> [Signal a] -> [Signal a]
dropSecs s = map f
    where f (Signal t1 t2 dt sf eq) = Signal (t1+s) t2 dt (SV.drop (round $ s/dt) sf) eq -- $ \p-> sf (p+(round $ s/dt))

labelMagically :: Double -> Int -> [Duration a] -> [Duration Int]
labelMagically ivl n durs | length durs < n = []  
                          | otherwise = 
                              let initDurs = take n durs 
                                  distPrev (t1:t2:rest) = 
                                      (t2-t1):distPrev (t2:rest)
                                  distPrev _ = []
                                  tolerence = ivl/10
                                  accept intvl = intvl >ivl-tolerence && 
                                                 intvl<ivl+tolerence
                                  dists = distPrev $ map (fst . fst) initDurs
                                  labels = tagMany [1..n] initDurs
                              in if all accept dists
                                    then labels ++ (labelMagically ivl n $ drop n durs)
                                    else labelMagically ivl n $ tail durs
--chiSquare :: [[Duration a]] -> 

habitAnal rep' spikes = let rep = filter (\d -> ( realToFrac . snd  . head $ countDuring [d] spikes) > 0.5) rep'
                        in (groupBy rep $ realToFrac <$$> countDuring rep spikes)  `groupStats` meanSDF

restrictDur (t1r, t2r) = map $ \((t1, t2), v) -> ((t1+t1r, t1+t2r), v)

histSig nbins evs = let (hlist, lo, hi, bin)= histList nbins $ map (getTag) evs
                        len = length hlist                        
                    in Signal lo hi bin (SV.pack hlist) Eq

histSigBZ bz evs  = let (hlist, lo, hi, bin)= histListBZ bz $ map (getTag) evs
                        len = length hlist                        
                    in Signal lo hi bin (SV.pack hlist) Eq

extendDur te = map (\((t1, t2), v)-> ((t1, t2+te), v))

spikeHistOver :: [Duration a] -> Double -> [Event b] -> [Signal Double]
spikeHistOver [] _ _ = [ConstSig 0]
spikeHistOver ds dt es = [(/(realToFrac $ length ds))`fmap` spikeHistOver' ds dt es]

spikeHistOver' :: [Duration a] -> Double -> [Event b] -> Signal Double
spikeHistOver' [] _ _ = ConstSig 0
spikeHistOver' (d@((t1, t2),_):durs) dt es = (hist (during [d] es)) + spikeHistOver' durs dt es
    where hist [] = ConstSig 0
          hist evs = let hlist= map (/dt) $ histListFixed t1 t2 dt $ map fst evs
                     in Signal 0 (t2-t1) dt (SV.pack hlist) Eq
        
histManyOver :: [Duration a] -> Double -> [Event b] -> [Signal Double]
histManyOver durs dt es = map h durs
    where h d@((t1,t2),_) = hist (during [d] es)
              where hist evs = let hlist= map (/dt) $ histListFixed t1 t2 dt $ map fst evs
                               in Signal t1 t2 dt (SV.pack hlist) Eq

integralOfSig s@(Signal t1 t2 dt _ _) =
    let sm = sum $ sigToList s
    in sm/realToFrac (t2-t1)

normSigToArea s = let igrl = integralOfSig s in (/igrl) `fmap` s

dist x y = abs $ x - y
closest x y z = if dist z x < dist x y
                   then z
                   else y
        
isSorted (y:(r@(z:xs))) | y>z = False
                        | otherwise = isSorted r
isSorted (y:[]) = True

--nearestTo :: Double -> [Double] -> Double
nearestTo :: (Ord t, Num t) => t -> [t] -> t
nearestTo x (y:(r@(z:xs))) | y<x &&z>=x = closest x y z
                           | z>x && y>x = closest x y z
                           | otherwise = nearestTo x r
nearestTo x (y:[]) = y

nearestToEach :: [Event a] -> [Event b] -> [Event Double]
nearestToEach mainEv otherEv = map f mainEv
    where f (t,_) = (t, dist t $ nearestTo t $ map fst otherEv) 

burst :: Double -> [Event a] -> [Duration ()]
burst tmax es = burst' Nothing tmax es
burst' :: Maybe Double -> Double -> [Event a] -> [Duration ()]
burst' (Nothing) tmax es@(e:[]) = []
burst' (Just tbstart) tmax ((t,v):[]) = [((tbstart, t), ())]
burst' (Just tbstart) tmax ((t1,v1):res@((t2,v2):es)) 
    | dist t1 t2 > tmax = ((tbstart, t1), ()) : burst' (Nothing) tmax res
    | otherwise = burst' (Just tbstart) tmax res
burst' (Nothing) tmax ((t1,v1):res@((t2,v2):es)) | dist t1 t2 < tmax = burst' (Just t1) tmax res
                                                 | otherwise = burst' (Nothing) tmax res


nearly epsilon x y = abs (x - y) < epsilon 

between x1 x2 x = x<max x1 x2 && x> min x1 x2

--contains arbitrary dt for histogram



contains :: (ChopByDur [a]) => [a] -> [Duration b] -> [Duration b]
contains evs durs = filter p durs
    where p dur = not . null $ concat $ chopByDur [dur] evs

notDuring :: ChopByDur [t] => [Duration a] ->  [t] -> [t]
notDuring durs evs = filter p evs
    where p e = null $ concat $ chopByDur durs [e]

minInterval :: HasTStart t => Double -> [t a] -> [t a]
minInterval t [] = []
minInterval t es@(e:[]) = es
minInterval t (ts1:res@(ts2:es)) | dist (gettStart ts1) (gettStart ts2) < t = minInterval t (ts1:es)
                                 | otherwise = ts1 : minInterval t res


eq4 x y = abs(x-y)<1e-4

evInDur t = shift t . durStart

reTimeSigsBy [] _ = []
reTimeSigsBy _ [] = []
reTimeSigsBy (((t1, t2),_):durs) (Signal ts1 ts2 dt arr eq:sigs) = (Signal t1 t2 dt arr eq):reTimeSigsBy durs sigs