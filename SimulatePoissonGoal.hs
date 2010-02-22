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
import QueryUtils

sample1 :: MonadIO m => Sampler a -> m a
sample1 sam = liftM head (io $ runSamplerIO sam)

trialRateSD = 20

amptrialsd = 20
ampsessmean = 200
ampsesssd = 200
baselinemean = 0.2
baselinesd = 0.1
adbl = always . NumV . NReal 

main = 
    forM_ [0..9] $ \i -> do
      deleteSessionIfExists $ "poisson"++show i
      inApproxSession ("new:poisson"++show i) $ do          
            amptrialmean <- sample1 $ gauss ampsessmean ampsesssd
            ntrials <- sample1 $ oneOf [20..100]
            simspikes <- useFile "SimulatePoissonSpikes" [("maxRate", realT)] []
            times ntrials $ do 
               amp <- sample1 $ gauss amptrialmean amptrialsd
               determine simspikes [("amp", adbl amp)]
            

