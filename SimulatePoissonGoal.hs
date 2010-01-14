module Main where

import QueryRun hiding (uniform,oneOf)
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
import Control.Monad.Trans

sample1 :: MonadIO m => Sampler a -> m a
sample1 sam = liftM head $ (io $ runSamplerIO sam)

poprate = 200
popratesd = 10
trialRateSD = 20
--baseline = 0.1
--tau = 0.2
--t0 = 5

main = forM_ [0..9] $ \i -> inApproxSession ("new:poisson"++show i) $ do          
         sessRate <- sample1 $ gauss poprate popratesd
         ntrials <- sample1 $ oneOf [20..100]
         intfire <- useFile "SimulatePoissonSpikes" [("maxRate", realT)] []
         times ntrials $ do 
           trialRate <- sample1 $ gauss sessRate trialRateSD
           determine intfire [("maxRate", always $ NumV $ NReal trialRate)]
         return ()


