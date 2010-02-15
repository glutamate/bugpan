{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}

module SpikeDetect where

--import System.Time
--import Database
--import Parse
--import EvalM hiding (ListT)
--import Eval
--import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans
--import Control.Monad.State.Lazy
--import HaskSyntaxUntyped
--import QueryUnsafe
--import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
--import ValueIO
--import Numbers
import TNUtils
import PlotGnuplot
import QueryPlots
import Data.List hiding (groupBy)
import Data.Ord
import System.Environment
import EvalM
import Numeric.LinearAlgebra hiding (flatten)
import Foreign.Storable
import Math.Probably.KMeans 
import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.IORef
import Database
import NewSignal

--main = allSpikes -- spikeDetectIO

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

--sinSig :: Signal Double
--sinSig = listToSig 0.1 0 $ map sin $ [0, 0.4..4]

--from hmatrix paper
mean :: (Field t, Num (Vector t)) => Matrix t -> Vector t
mean x = sumColumns x / fromIntegral (rows x)
    where sumColumns m = constant 1 (rows m) <> m


cov :: (Field t, Num (Vector t)) => Matrix t -> Matrix t
cov x = (trans xc <> xc) / fromIntegral (rows x -1)
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m

--bishop 2.43 (p 78)

unUnitList [x] = x

matToScalar :: Matrix Double -> Double
matToScalar = unUnitList . unUnitList . toLists

multiGauss :: (Vector Double, Matrix Double) -> Vector Double -> Double
multiGauss (mu,sigma) x = 
    let d = realToFrac $ length $ toList mu
        xMinusSigma = fromColumns [x-mu]
        logLike =(log $ recip ((2*pi)**(d/2)) * recip (sqrt (det sigma))) + (negate . (/2) . matToScalar $ trans (xMinusSigma) <> inv sigma <> (xMinusSigma)) 
    in logLike

fitMultiGauss :: [Vector Double] -> (Vector Double, Matrix Double)
fitMultiGauss xs = (mean $ fromRows xs, cov $ fromRows xs)

--multiGaussMixture :: [(Double,(Vector Double, Matrix Double))] -> Vector Double -> Double
multiGaussMixture pimusigmas x =  sum $ map (\(pi,gauss)-> pi * (gauss `multiGauss` x)) pimusigmas


testMeans = 2 |> [0,0]
testSigma = (2><2) [2, -1,
                    -1, 1]

testval = multiGauss (testMeans, testSigma) $ 2 |> [1,1]
                     
testObs :: [Vector Double]
testObs = [fromList [1,20,30],                    
           fromList [2,31,41],
           fromList [3,32,45],
           fromList [4,33,43]]


zipWithMap :: (a->b) -> [a] -> [(a,b)]
zipWithMap f xs = map (\x->(x, f x)) xs

  {-let (m1::Matrix Double) = (2><2) [ 1, 1,
                                     -2,4]
  io $ print $ eig m1-}

vecSubMean vec = let lst = toList vec
                     mean = runStat meanF lst
                     submean x = x-mean
                 in mapVector submean vec
                 

--runStatVec :: (Storable b) => Fold b c -> Vector b -> c
--runStatVec (F f x c _) v = c . (foldVector f x)

listToPoint (x:y:_) = (x,y)

indexMany :: [a] -> [Int] -> [a]
indexMany = map . (!!) 


sigStarts :: [Signal a] -> [Event ()]
sigStarts = map (\(Signal t1 _ _ _ _) -> (t1,()))

shiftEach :: [Double] -> [Event a] -> [Event a]
shiftEach [] _ = []
shiftEach _ [] = []
shiftEach (s:ss) ((t,v):evs) = (t+s, v): shiftEach ss evs

takeEvery n [] = []
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)


listToTips = map Leaf

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node tl tr) = flatten tl ++flatten tl

data Tree a = Leaf a | Node (Tree a) (Tree a)

zipWithNats :: [a] -> [(a,Int)]
zipWithNats xs = zip xs [0..]

zipWithNatGrid :: [[a]] -> [[(a,(Int,Int))]]
zipWithNatGrid  xss = map f $ zipWithNats xss
    where f (xs,rowN) = map (g rowN) $ zipWithNats xs
          g rowN (x,colN)= (x, (rowN, colN))

flattenGrid :: [[a]] -> [a]
flattenGrid = concat

nukeDiag :: (Num a, Ord a) => [[(a,(Int,Int))]] -> [[(a,(Int,Int))]]
nukeDiag xss = let high = fst $ (maximumBy $ comparing fst) $ map (maximumBy $ comparing fst) xss
                   f (x, (r,c)) | r==c = (high+1, (r,c))
                                | otherwise = (x, (r,c))
               in map (map f) xss

hierarchicalCluster :: ([a] -> [a] -> Double) -> [a] -> Tree a
hierarchicalCluster dist xs = h $ listToTips xs
    where h [tree] = tree
          h ts = let dists = nukeDiag $ zipWithNatGrid $ interaction dist $ map flatten ts
                     (minr,minc) = snd $ minimumBy (comparing fst) $ flattenGrid dists
                     ([x,y], ts') = partition (\(tree, idx)->idx == minr || idx == minc) $ zipWithNats ts
                 in h $ Node (fst x) (fst y) : map fst ts' 

interaction :: (a->a->b) -> [a] -> [[b]]
interaction dist xs = map  (\x-> map (dist x) xs) xs

euclidianOn f v1 v2 = sqrt $ sum $ toList $ mapVector (\x->x*x) (f v1 - f v2)

meanDists :: (a->a->Double) -> ([a] -> [a] -> Double)
meanDists f xs ys = let nxy = realToFrac $ length xs * length ys
                        sm = sum $ map sum $ map g xs
                        g x = map (f x) ys
                    in sm/nxy

eventDetect :: Int -> [Duration Double] -> [Signal Double] -> [Event Int]
eventDetect nclusters ((_,thresh):_) sigs = 
    let sd = stdDevF `sigStat` sigs
        putatives = if thresh <0  
                        then crossesDown (((*thresh) . negate) <$$> sd) sigs 
                        else crossesUp ((*thresh) <$$> sd) sigs
        waveforms =  limitSigs' (-0.0005) 0.0005 $ around putatives $ sigs
        alignWaveforms = --limitSigs' (-0.0006) 0.0006 $ 
                         unjitter $ 
                         alignBy (centreOfMass . ((square . square) <$$>) ) $ 
                         waveforms 
        alignedEvents = shiftEach (map (negate . (+0.0005). fst) $ sigStarts alignWaveforms) putatives
        realignedWaveforms = limitSigs' (-0.0005) 0.0005 $ around alignedEvents sigs
        neigenVecs = 5
        dataMatrix = fromLists $ map sigToList $  realignedWaveforms
        datMatSubMeans = fromColumns $ map vecSubMean $ toColumns dataMatrix
        covDM = cov datMatSubMeans
        (eigvals, eigvecs) = eig covDM
        evs = take neigenVecs $ map snd $ reverse $ 
              sortBy (comparing fst) $ 
              zip (fmap realPart $ toList eigvals) [0..]
        featureVector = fromColumns $ map (mapVector realPart . ((toColumns eigvecs)!!)) evs
        finalData = trans featureVector <> trans datMatSubMeans
        pts =  map toList $ toColumns $ finalData
        nobs = realToFrac $ length $ alignWaveforms
        clustered = kmeansOn (snd) nclusters $  zip [0..] pts
        clusteredvs = map (map (listToPoint . snd)) clustered
        --gaussians = map (fitMultiGauss . map fromList) $ map (map snd) clustered
        --weights = map ((/nobs) . realToFrac . length . map snd) clustered
        --wgausses = zip weights gaussians
        --likelihood = sum $map (wgausses `multiGaussMixture`) $ map (fromList) pts
        --nparams = (realToFrac $ nclusters*neigenVecs*(neigenVecs*(neigenVecs-1)))/2 --means, covariance matrix          
        --bic = -2*likelihood +nparams*log nobs
        idxs = map (map fst) clustered
        sigsav = map (take 1 . averageSigs . (alignWaveforms `indexMany`)) idxs
        evss = sortBy (comparing fst) $ concatMap (\(es,i)-> i `tag` es ) $ zip (map (minInterval 0.001 . (alignedEvents `indexMany`)) idxs) [0..]
        realnclust = length clusteredvs

    in evss


{-allSpikes = do
  {-inApproxSession "6f" $ do
              sigs <- take 20 `fmap` signalsDirect "normV"
              let spks = eventDetect 12 sigs
              storeAsOvwrt "spikeClusters2" spks
              return () -}
  inEverySession $ do
              Session bdir _ <- getSession
              io$ print bdir
              normV <- signalsDirect "normV"
              --let normV = subMeanNormSD sigs
              --storeAsOvwrt "normV" normV
              let spks = eventDetect 12 (durd $ -9) normV
              storeAsOvwrt "spikeClusters" spks
              return ()


-}
{-spikeDetectIO = do 
  (snm:overDurNm:_) <-getArgs 
  inApproxSession snm $ do 
                  --openReplies
                  initUserInput
                  plotSize 490 329
                  overDur <- unitDurations overDurNm
                  --autoSpikes "normV"
                  return ()
                  --normV <- signalsDirect "normV"
                  --spikeDetect [overDur] normV []

spikeDetect overs normV spks = do
  let over = head overs
  io $ putStrLn $ "currently considering "++show (length over)++" durations, "++show (length spks)++" spikes"
  userChoice [('s', "show normV and existing spikes", 
                  do --normV <- signalsDirect "normV"
                      spikes <- events "spikes" () 
                      ask $ plotManyBy over $ normV :+: ("stored spikes", (-10) `tagd` spikes) :+: 
                              ("New spikes", (-12) `tagd` spks)
                      spikeDetect overs normV spks),
              ('u', "undo restrict",
                   case overs of 
                     [o] -> spikeDetect overs normV spks
                     o:os->spikeDetect os normV spks),
              ('c', "clear detected spikes",
                    spikeDetect overs normV []),
              ('q', "quit",
                  return ()),
              ('d', "delete stored spikes",
                  do deleteValue "spikes"
                     spikeDetect overs normV spks),
              ('v', "save detected spikes",
                    do storeAsOvwrt "spikes" $ sortBy (comparing fst) spks
                       spikeDetect overs normV spks),
              ('r', "restrict trials", 
                   do ndrop <- userValue "number to drop"
                      ntake <- userValue "number to take"
                      spikeDetect ((take ntake $ drop ndrop over):overs) normV spks),
              ('f' , "fixed threshold", 
                   do thr <- userValue "threshold"
                      let thresh = durd (thr)
                      let spikes = during over $ crossesDown thresh normV
                      ask $ plotManyBy over $ normV :+: thresh :+: thr `tagd` spikes
                      ifM (userConfirm "accept spikes")
                          (spikeDetect overs normV (spks++spikes))
                          (spikeDetect overs normV spks))]  
                                         -}  

