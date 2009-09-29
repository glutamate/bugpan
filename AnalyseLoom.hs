{-# LANGUAGE BangPatterns, Rank2Types #-}

module Main where

--import System.Time
--import Database
--import Parse
--import EvalM hiding (ListT)
--import Eval
--import Expr
--import Stages
import Query
import QueryTypes
import Control.Monad.Trans hiding (lift)
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
import Database
import Numeric.FAD

main = loomAnal

atMost x = min x
atLeast x = max x

--ask q = liftIO$ qReply q []

loomAnal = do ress <- inEverySession $ do  
                        snm <- (last . splitBy '/' . baseDir) `fmap` getSession
                        rep <- durations "repetition" (1::Int)
                        rearLoc <- durations "rearingLocation" ""
                        spikes <- events "spikes" ()                        
                        let spks = during (restrictDur (0,5) rep) spikes
                        let numSpks = (fst . snd) `fmap` habitAnal rep spks
                        return $ ((take 4  snm, snd . head $ rearLoc), numSpks)
              let (up, down) = partition ((=="upstairs") . snd. fst) ress
              
              let plotUp = foldl (.+.) (GnuplotBox Noplot) $ map plLin up
              let plotDown = foldl (.+.) (GnuplotBox Noplot) $ map plDash down
              gnuplotOnScreen  $ plotUp 
              gnuplotOnScreen  $ plotDown
              gnuplotToPNG "habitMeans.png" $ ("upstairs", meanPlot up) :+: 
                                              ("downstairs", meanPlot down) :+: 
                                              ("all", meanPlot ress)
              --liftIO $ mapM_ fitexp ress
    where plLin ((snm, loc), spks) =  (snm ++ " "++loc, Lines (zip wholeReals $ spks) :+: fitexp spks) 
          plDash ((snm, loc), spks) =  (snm ++ " "++loc, Lines (zip wholeReals $ spks) :+: fitexp spks)
          mean xss@(xs:_) = map (\i-> (sum $ map (!!i) xss)/ (realToFrac $length xss)) $ map fst $ zip [0..] xs
          meanPlot res = Lines $ zip wholeReals $  mean $ map snd res
          x .+. y = GnuplotBox $ (x) :+: (y)
          norm67 xs@(x:_) = map (\y -> 67*y/x) xs
          fitexp spks = --do
            --putStrLn $ snm ++ " "++loc
            let pts = zip [0..] spks
--            let pars= fit expDecay pars [10, 2, 20]
            --let f args = sum $ map (\(t,y)-> (expDecay t args - lift y)**2) pts
            --let p2 = argminNaiveGradient f [10, 2, 20]
                pars= (!!201) $ fit expDecay (pts) [100, 2, 20] in
            --print $ take 10 $ p2
            FunSeg 0 30 $ fitFun pars expDecay
wholeReals = [(0::Double)..]

fit :: (Fractional a, Ord a, Floating a) => (forall tag. b -> [Dual tag a] -> Dual tag a) -> [(b,a)] -> [a] -> [[a]]
fit g pts p0 = let ss args = sum $ map (\(t,y)-> (g t args - lift y)**2) pts
               in argminNaiveGradient ss p0

--expDecay :: (Floating a) => a -> [a] -> a
expDecay1 [t, a, tau, s0]  = a*exp(-t*tau)+s0
--expDecay :: (Fractional a, Ord a, Floating a) =>  Double -> (forall tag. [Dual tag a] -> Dual tag a)
expDecay :: (Floating b, Real a) => a -> [b] -> b
expDecay t [a, tau, s0]  = a*exp(-(realToFrac t)/tau)+s0

fitFun :: [a] -> (b -> [a] -> a) -> (b->a)
fitFun pars f = \t->f t pars 