{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}

module Main where

import System.Environment
import Data.Binary
import System.Directory
import Data.List
import TNUtils
import Data.Maybe
import Control.Monad
import Math.Probably.FoldingStats
import StatsModel
import QueryPlots


main = do
  nm:thinnStr:_ <- getArgs
  parnms::[String] <- read `fmap` readFile (nm++"_parnames.mcmc") 
  files <- getFiles nm
  let chains = nub . fst $ unzip files
  let npars = length parnms
  writeFile (nm++"thin_parnames.mcmc") $ show parnms 
  forM_ chains $ \c-> do
          let fls = take 100 $ sort $ lookupMany c files
          xs::[[Double]] <- fmap concat $ forM fls $ \fl-> do
                  (thin $ read thinnStr) `fmap` safeLoad (unparseFileName nm c fl)
          writeInChunks (nm++"thin_chain"++show c) 20000 xs

          
 

 
showError ((mu, sd), jf) = "(JF "++accushow (100*jf)++"%) "++accushow mu ++ " +/- " ++accushow sd

padStr n s | n > length s = s ++ replicate (n-length s) ' '
           | otherwise = s