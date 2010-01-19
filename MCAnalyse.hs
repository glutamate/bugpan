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
          print file
          ifM (doesFileExist file ) 
             (Just `fmap` loadBinary file)
             (return Nothing)
  let bigList = (concat $ catMaybes xs) :: [[Double]]
  when ("-j" `elem`restArgs) $ do
         putStrLn $ "jump frequency: "++ show (jumpProbBy (nearlyEq 1e-9) bigList)
       
  putStrLn $ "#values="++show (length bigList)
  parstr <- readFile (nm++"_parnames.mcmc") 
  forM (zip (meanSDF `runStatOnMany` bigList) (read parstr)) $ \(estim, pnm) -> putStrLn $ padStr 20 (pnm ++ ": ")++showError estim 

showError (mu, sd) = show mu ++ " +/- " ++show sd

padStr n s | n > length s = s ++ replicate (n-length s) ' '
           | otherwise = s