{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances #-}
{-# OPTIONS_GHC -fvia-c -optc-O3 #-}
module StatsModel where

import HaskSyntaxUntyped
import Expr
import EvalM
import Numbers
import System.Environment
import Data.Char
import Data.List
import TNUtils
import Query 
import Control.Monad
import Math.Probably.Sampler
import Math.Probably.StochFun
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import QueryTypes
import Math.Probably.FoldingStats
import PlotGnuplot
import QueryPlots
import QueryUtils hiding (groupBy)
import Database
import Data.Array.Vector 
import Data.Binary
import GHC.Conc


writeInChunks :: Binary a => String -> Int -> [a] -> IO ()
writeInChunks = writeInChunks' 0
    where writeInChunks' _ _ _ [] = return ()
          writeInChunks' counter fnm chsize xs = do
            let (out, rest) = splitAt chsize xs
            saveBinary (fnm++"_file"++(show counter)++".mcmc") out
            writeInChunks' (counter+1) fnm chsize rest
    

bigSigma :: Num b =>  [a] -> (a->b)-> b
bigSigma xs f = sum $ map f xs


manyLikeH :: (ChopByDur obs,Shiftable obs) => 
             [Duration [Int]] -> 
            ([Int] -> obs -> P.PDF theta) -> 
            (obs -> P.PDF theta)
manyLikeH durs lh1 obs = 
    let z = zip (chopAndReset durs obs) durs
    in \theta-> sum $ map (\(obs, (_,ints)) -> lh1 ints obs theta) $ z

within :: [Duration [Int]] -> [Duration [Int]] -> [Duration [Int]]
within short long = concatMap f $ relabelWithin long short 
    where f d@(t1t2, sints) = case sectionDur1 d long of 
                                  []-> []
                                  (_,lints):_ -> [(t1t2, lints++sints)]

distinct :: [Duration a] -> [Duration [Int]]
distinct durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]


relabelWithin :: [Duration a] -> [Duration [Int]] -> [Duration [Int]]
relabelWithin long short = concatMap (f . (:[])) long
    where f onelong = (:[]) <$$> (tagMany [0..] $ during onelong short)

--distinctWithin :: [Duration a] -> [Duration [Int]]
--distinctWithin durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]

--non-heirarchical
manyLikeOver :: (ChopByDur obs,Shiftable obs) => [Duration a] -> (theta -> P.PDF obs) -> (obs -> P.PDF theta)
manyLikeOver durs lh1 = \obs-> \theta-> sum $ map (lh1 theta) $ chopAndReset durs obs 


class MutateGaussian a where
    mutGauss :: Double -> a -> Sampler a
    mutGaussMany :: Double -> [a] -> Sampler [a]
    mutGaussMany cv = mapM (mutGauss cv) 
    nearlyEq :: Double -> a -> a -> Bool

instance MutateGaussian Double where
    mutGauss cv x = gauss x (cv*x)
    mutGaussMany cv xs = gaussMany (map (\x-> (x,cv*x)) xs)
    nearlyEq tol x y = abs(x-y)<tol  

instance MutateGaussian a => MutateGaussian [a] where
    mutGauss cv xs = mutGaussMany cv xs 
    nearlyEq tol xs ys = length xs == length ys && (all (uncurry $ nearlyEq tol) $ zip xs ys )

instance MutateGaussian (UArr Double) where
    mutGauss cv xs = toU `fmap` mutGaussMany cv (fromU xs)
    nearlyEq tol xs ys = lengthU xs == lengthU ys && (allU (uncurryS $ nearlyEq tol) $ zipU xs ys )

instance (MutateGaussian a, MutateGaussian b) => MutateGaussian (a,b) where
    mutGauss cv (x,y) = liftM2 (,) (mutGauss cv x) (mutGauss cv y)
    nearlyEq t (x,y) (x1,y1) = nearlyEq t x x1 && nearlyEq t y y1

instance (MutateGaussian a, MutateGaussian b, MutateGaussian c) => MutateGaussian (a,b,c) where
    mutGauss cv (x,y,z) = liftM3 (,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z)
    nearlyEq t (x,y, z) (x1,y1, z1) = nearlyEq t x x1 && nearlyEq t y y1 && nearlyEq t z z1


instance (MutateGaussian a, MutateGaussian b, MutateGaussian c, MutateGaussian d) => MutateGaussian (a,b,c,d) where
    mutGauss cv (x,y,z,w) = liftM4 (,,,) (mutGauss cv x) (mutGauss cv y) (mutGauss cv z) (mutGauss cv w)
    nearlyEq t (x,y, z, w) (x1,y1, z1, w1) = nearlyEq t x x1 && nearlyEq t y y1 && nearlyEq t z z1 && nearlyEq t w w1

instance ChopByDur (UArr Double) where
    chopByDur durs arr = map (\((t1,t2),_)->filterU (\t->t>t1 && t<t2 ) arr) durs

instance Shiftable (UArr Double) where
    shift ts = mapU (+ts)
    rebaseTime = undefined

jumpProbBy :: (a -> a -> Bool) -> [a] -> Double
jumpProbBy eqf xs = jPB xs 0 0 
    where jPB (x:xs@(y:_)) js tots | eqf x y = jPB xs (js+1) (tots+1)
                                   | otherwise = jPB xs js (tots+1)
          jPB _ jumps total = realToFrac jumps / realToFrac total

inPar :: Int -> (Int -> IO ()) -> IO ()
inPar 0 ma = return ()
inPar 1 ma = ma 0
inPar n ma = do
  tids <- forM [0..n-1] $ \i -> forkIO (ma i)
  loop tids
    where loop tds = do
               threadDelay $ 200000
               tss <- mapM threadStatus tds
               if all (okStatus) tss
                  then return ()
                  else loop tds
          okStatus ThreadFinished = True
          okStatus ThreadDied = True
          okStatus _ = False

  

instance Shiftable Double where
    shift = (+)
    rebaseTime = undefined



lastn n xs = let len = length xs 
             in if n > len
                   then xs
                   else drop (len - n) xs


--between l u x = x > l && x < u


--mapIdx :: (Int -> b) -> [a] -> [b]
--mapIdx f xs = map f [0..length xs-1]
