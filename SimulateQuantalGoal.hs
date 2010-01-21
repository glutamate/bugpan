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


nmax = 20
np = 0.25

qmean = 20
qsd = 5

pmean = 0.4
psd = 0.2

pupfactormean = 2
pupfactorsd = 0.4

pdownfactormean = 2
pdownfactorsd = 0.4

cv1 = 0.2

main = forM_ [0..9] $ \i -> do
         deleteSessionIfExists $ "quantal"++show i
         inApproxSession ("new:quantal"++show i) $ do                                        
                              ntrials <- sampleN 3 $ oneOf [20..100]
                              simsyns <- useFile "SimulateQuantalSyns" [] []
                              nsess <- sample1 $ binomial nmax np
                              psess <- sample1 $ gaussD pmean psd
                              qsess <- sample1 $ gaussD qmean qsd
                              pupsess <- sample1 $ gauss pupfactormean pupfactorsd
                              pdownsess <- sample1 $ gauss pdownfactormean pdownfactorsd
                              let doTrial pseg = do 
                                           determine simsyns []
                                           nrel <- sample1 $ binomial nsess pseg
                                           qs <- sampleN nrel (gaussD qsess (cv1*qsess))
                                           inLast $ "epsc" := ( sum qs )
                              times (ntrials!!0) $ doTrial $ psess
                              times (ntrials!!1) $ doTrial $ psess*pupsess
                              times (ntrials!!2) $ doTrial $ psess/pdownsess
                              return ()

gaussWithCV cv mean = gaussD mean (mean*cv)

sample1 :: MonadIO m => Sampler a -> m a
sample1 sam = liftM head $ (io $ runSamplerIO sam)

sampleN :: MonadIO m => Int -> Sampler a -> m [a]
sampleN n sam = liftM (take n) $ (io $ runSamplerIO sam)



