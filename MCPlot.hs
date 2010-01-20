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
import Text.Regex.Posix
import PlotGnuplot
import QueryPlots

parseFileName :: String -> String -> Maybe (Int, Int)
parseFileName setnm filenm = 
    let mat = "^"++setnm++"_chain(.+)_file(.+).mcmc$" in -- [[mat, filenm]]
    case filenm =~ mat of 
      [[all, cnum, fnum]]-> liftM2 (,) (safeRead cnum) (safeRead fnum)
      _ -> Nothing

unparseFileName :: String -> Int -> Int -> String
unparseFileName setnm cnum fnum = setnm++"_chain"++show cnum++"_file"++show fnum++".mcmc"

tst :: Bool
tst =  "foo_bar" =~ "foo_bar" 
       
thin :: Int -> [a] -> [a]
thin n [] = []
thin 0 xs  = xs
thin n xs = let (x:_, ys) = splitAt (n+1) xs
            in x : thin n ys
            


isLstDbls :: [Double] -> [Double]
isLstDbls = id
                         
main = do
  nm:parnm:restArgs <- getArgs
  parstr <- readFile (nm++"_parnames.mcmc") 
  let Just parIdx = fmap snd $ find ((==parnm) . fst) $ zip (read parstr) [0..]
  files <- (catMaybes . map (parseFileName nm) . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  let chains = nub . fst $ unzip files
  --mapM print files
  cs <- forM chains $ \c-> do
          let fls = sort $ lookupMany c files
          fmap concat $ forM fls $ \fl-> do
             fmap (map (!!parIdx) . thin 10) $ loadBinary (unparseFileName nm c fl)

  gnuplotOnScreen $ map GnuplotBox $ map (zip [(0::Double), 10..] . isLstDbls) cs
  {-let (lo, hi) = (safeHead restArgs >>= safeRead) `orJust` (0,nfiles)
  xs <- forM [0..hi] $ \fnum-> do 
          let file =  (nm++"_chain"++chain++"_file"++show fnum++".mcmc")
          print file
          ifM (doesFileExist file ) 
             (Just `fmap` loadBinary file)
             (return Nothing)
  let bigList = (concat $ catMaybes xs) :: [[Double]]
  when ("-j" `elem`restArgs) $ do
         putStrLn $ "jump frequency: "++ show (jumpProbBy (nearlyEq 1e-8) bigList)
       
  putStrLn $ "#values="++show (length bigList)
  parstr <- readFile (nm++"_parnames.mcmc") 
  forM (zip (meanSDF `runStatOnMany` bigList) (read parstr)) $ \(estim, pnm) -> putStrLn $ padStr 20 (pnm ++ ": ")++showError estim 
-}


showError (mu, sd) = show mu ++ " +/- " ++show sd

padStr n s | n > length s = s ++ replicate (n-length s) ' '
           | otherwise = s