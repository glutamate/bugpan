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
import Numeric.LinearAlgebra
import Data.Maybe
import Control.Monad
import Control.Arrow
import Control.Applicative

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
    where plLin ((snm, loc), spks) =  (snm++" "++loc, Lines (zip wholeReals $ spks) :+: fitexp spks) 
          plDash ((snm, loc), spks) = (snm++" "++loc, Lines (zip wholeReals $ spks) :+: fitexp spks)
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

fit :: (Ord a, Floating a) => (forall tag. b -> [Dual tag a] -> Dual tag a) -> [(b,a)] -> [a] -> [[a]]
fit g pts p0 = let ss args = sum $ map (\(t,y)-> (g t args - lift y)**2) pts
               in argminNaiveGradient ss p0

--expDecay :: (Floating a) => a -> [a] -> a
expDecay1 [t, a, tau, s0]  = a*exp(-t*tau)+s0
--expDecay :: (Fractional a, Ord a, Floating a) =>  Double -> (forall tag. [Dual tag a] -> Dual tag a)
expDecay :: (Floating b, Real a) => a -> [b] -> b
expDecay t [a, tau, s0]  = a*exp(-(realToFrac t)/tau)+s0

fitFun :: [a] -> (b -> [a] -> a) -> (b->a)
fitFun pars f = \t->f t pars 

type LinModel a = (Matrix a, Matrix a)

paramCount :: Element a => LinModel a -> Int
paramCount (x,_) = length . head $ toLists x

observationCount (_,y) = length . head . toLists $ trans y

--http://en.wikipedia.org/wiki/Akaike_information_criterion#AICc_and_AICu
aic  model = let n = realToFrac $ observationCount model
                 k = realToFrac $ paramCount model
             in log (ss model/n) +(n+k)/(n-k-2)

--http://en.wikipedia.org/wiki/Bayesian_information_criterion
bic model =  let n = realToFrac $ observationCount model
                 k = realToFrac $ paramCount model
             in n*log (ss model/n) +k*log n


lm :: (Matrix Double , Matrix Double) -> Matrix Double
lm (x,y) = inv (trans x<>x) <> (trans x) <> y

ss :: LinModel Double -> Double
ss (x,y) =  sum . map (\x->x*x) . head . toLists . trans $ y - x<>lm (x,y)

oneMean :: [Double] -> LinModel Double
oneMean xs = let xm = fromLists $ map (\x-> [1]) xs                 
             in (fromBlocks [[xm]], trans . fromLists $ [xs])

twoMeans :: [Double] -> [Double] -> LinModel Double
twoMeans xs ys = let xm = fromLists $ map (\x-> [1, 0]) xs
                     ym = fromLists $ map (\x-> [0, 1]) xs
                 in (fromBlocks [[xm],
                                 [ym]], trans . fromLists $ [xs++ys])

regression :: [(Double, Double)] -> LinModel Double
regression pairs = let n = length pairs
                       xs = fromLists $ [map fst pairs]
                       vecy = fromLists $ [map snd pairs]
                       ones = fromLists $ [replicate n 1]
                   in (trans . fromBlocks $ [[ones],
                                             [xs]], trans vecy)

tm = twoMeans [1,2, 3] [4,5,6]
reg = regression [(1,1), (2,2), (3,3), (4,4)]

instance Num Factor where
    f1 + f2 = Plus f1 f2
    f1 * f2 = Nest f1 f2
    fromInteger x = ConstFactor $ realToFrac x
    abs = undefined
    signum = undefined

data Factor = Categorical [Int]
            | Continuous [Double]
            | ConstFactor Double
            | Mean Int
            | Nest Factor Factor
            | Plus Factor Factor
            | Cut Factor Double
             deriving (Show, Eq)

newtype Parser t a = Parser { unParser :: [t] -> [(a,[t])] }

instance Functor (Parser t) where
    fmap f (Parser df) = Parser $ \dbls -> map (\(x,unconsumed) -> (f x, unconsumed)) $ df dbls

instance Applicative (Parser t) where
    pure x = Parser $ \dbls -> [(x,dbls)]
    (Parser df1) <*> (Parser df2) = 
        Parser $ \dbls -> concatMap (\(f,uncons) -> map (first f) $ df2 uncons ) $ df1 dbls  

instance Monad (Parser t) where
    return  = pure 
    (Parser df) >>= f = 
        Parser $ \dbls-> concatMap (\(x,uncons) -> unParser (f x) uncons ) $ df dbls  
    fail s = Parser $ \_ -> []

instance MonadPlus (Parser t) where
    mzero = Parser $ \_ -> []
    mplus (Parser df1) (Parser df2) = 
        Parser $ \strs -> case df1 strs of
                            [] -> df2 strs
                            xs -> xs

oneP = Parser $ \(x:xs) -> [(x,xs)]
manyP n = Parser $ \(xs) -> [splitAt n xs]


parsepars (Mean _) = show `fmap` oneP
parsepars (Continuous _) = do x<- oneP
                              return $ show x ++ "*x"
parsepars (Categorical ns) = do xs<- manyP $ length ns
                                return $ intercalate " + " $ map (\(x,n)-> show n++":"++show x) $ zip xs ns

parsepars (Plus f1 f2) = do x1 <- parsepars f1
                            x2 <- parsepars f2
                            return $ x1 ++ " + "++x2

runParser :: Parser t a -> [t] -> a
runParser (Parser f) s = fst . head $ f s

parseParams :: [Double] -> Factor -> String
parseParams pars facs = runParser (parsepars facs) pars


prettyResult :: Matrix Double -> Factor -> String
prettyResult lmres facts = 
    let params = toList $ head $ toRows lmres
    in show params

xs .<. cut = Cut xs cut

ys <~ facs = facModel (ys, facs)
facModel :: ([Double], Factor) -> LinModel Double
facModel (ys, facs') = let facs = normalForm facs'
                           dm = designMatrix facs
                       in (dm, trans $ fromLists [ys])

designMatrix :: Factor -> Matrix Double
designMatrix (Mean n) = trans $ fromLists $ [replicate n 1]
designMatrix (Continuous xs) = trans $ fromLists $ [xs]
designMatrix (Categorical xs) = let levels = sort $ nub $ xs
                                    nlevels = length levels
                                    zeros = repeat 0
                                    oneAt n = take n zeros ++ [1]++ take (nlevels-n-1) zeros
                                    gen x =oneAt $ fromJust $ elemIndex x levels
                                    lstMat = map gen xs
                                in  fromLists lstMat
designMatrix (Plus f1 f2) = fromBlocks [[designMatrix f1, designMatrix f2]]
designMatrix (Nest (Categorical xs) f) = let levels = sort $ nub $ xs
                                             nlevels = length levels
                                             dmf = toLists $ designMatrix f
                                             nfactors = length $ head dmf
                                             zeros = replicate nfactors 0
                                             lstMat = map (fromLists . gen) levels
                                             gen lvl = map (\(x, i)-> if x ==lvl
                                                                         then dmf!!i
                                                                      else zeros) $ zip xs [0..]                      
                                         in fromBlocks $ [lstMat]


validateModel f = True

normalForm (Nest c@(Categorical is) (Plus f1 f2)) = Plus (Nest c (normalForm f1)) (Nest c (normalForm f2))
normalForm (Nest f1 f2) = Nest (normalForm f1) (normalForm f2)
normalForm (Plus f1 f2) = Plus (normalForm f1) (normalForm f2)
normalForm (Cut (Continuous xs) cut) = Categorical $ map (\x-> if x < cut then 1 else 2) xs
normalForm primFac = primFac

