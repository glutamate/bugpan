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

help = do
  putStrLn "MCAnalyes {chain name} {chain number}"
  putStrLn "MCAnalyes {chain name} {chain number} '(lo,hi)'"



main = do 
  args <- getArgs
  case args of 
    [] -> help
    _ -> main1 

main1 = do
  nm:chain:restArgs <- getArgs
  nfiles <- (length . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  print nfiles
  let (lo, hi) = (safeHead restArgs >>= safeRead) `orJust` (0,nfiles)
  xs <- forM [lo..hi] $ \fnum-> do 
          let file =  (nm++"_chain"++chain++"_file"++show fnum++".mcmc")
          putStr $ file++" "
          ifM (doesFileExist file ) 
              (safeLoad file)             
              (return [])
  let bigList = (concat xs) :: [[Double]]
  putStrLn $ "#values="++show (length bigList)
  parstr <- readFile (nm++"_parnames.mcmc") 
  let estims = (both meanSDF (jumpFreqByF $nearlyEq 1e-8)) `runStatOnMany` bigList
  forM_ (zip estims (read parstr)) $ \(estim, pnm) -> putStrLn $ padStr 20 (pnm ++ ": ")++showError estim 

showError ((mu, sd), jf) = "(JF "++accushow (100*jf)++"%) "++accushow mu ++ " +/- " ++accushow sd

--padStr n s | n > length s = s ++ replicate (n-length s) ' '
--           | otherwise = s