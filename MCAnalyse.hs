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

main = do
  nm:chain:restArgs <- getArgs
  nfiles <- (length . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  let (lo, hi) = (safeHead restArgs >>= safeRead) `orJust` (0,nfiles)
  xs <- forM [lo..hi] $ \fnum-> do 
          let file =  (nm++"_chain"++chain++"_file"++show fnum++".mcmc")
          putStr $ file++" "
          ifM (doesFileExist file ) 
              (safeLoad file)             
              (return [])
  let bigList = thin 100 $ (concat xs) :: [[Double]]
  --when ("-j" `elem`restArgs) $ do
  --       putStrLn $ "jump frequency: "++ show (jumpProbBy (nearlyEq 1e-8) bigList)
       
  --putStrLn $ "#values="++show (length bigList)
  parstr <- readFile (nm++"_parnames.mcmc") 
  let estims = meanSDF `runStatOnMany` bigList
  forM_ (zip estims (read parstr)) $ \(estim, pnm) -> putStrLn $ padStr 20 (pnm ++ ": ")++showError estim 

showError (mu, sd) = show mu ++ " +/- " ++show sd

padStr n s | n > length s = s ++ replicate (n-length s) ' '
           | otherwise = s