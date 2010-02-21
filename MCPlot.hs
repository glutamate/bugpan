{-# LANGUAGE ScopedTypeVariables #-}

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
       


isLstDbls :: [Double] -> [Double]
isLstDbls = id

idx2 ix1 ix2 xs = (xs!!ix1, xs!!ix2)

main = do
  args <- getArgs
  case length args of
    0 -> help
    1 -> main0
    2 -> main1
    3 -> main2

help = do
  putStrLn "MCPlot {chain name}"
  putStrLn "MCPlot {chain name} {parameter name}"
  putStrLn "MCPlot {chain name} {parameter name} {parameter name}" 

main0 = do
  nm:_ <- getArgs
  parnms::[String] <- read `fmap` readFile (nm++"_parnames.mcmc") 
  files <- (catMaybes . map (parseFileName nm) . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  let chains = nub . fst $ unzip files
  let npars = length parnms
  cs:: [[[Double]]] <- forM chains $ \c-> do
          let fls = take 100 $ sort $ lookupMany c files
          fmap concat $ forM fls $ \fl-> do
             safeLoad (unparseFileName nm c fl)
  print parnms
  let plotpair i j | i>j = GnuplotBox Noplot
                   | i==j = GnuplotBox $ CentreLabel (parnms!!i)
                   | i<j = GnuplotBox $ for cs $ \c-> GnuplotBox $ zip (map (!!i) c) (map (!!j) $ thin 100 c)
  let plots = for [0..npars-1] $ \i-> for [0..npars-1] $ plotpair i
  gnuplotOnScreen $ gridPlot plots
  return ()
 
main1 = do
  nm:parnm1:_ <- getArgs
  parstr <- readFile (nm++"_parnames.mcmc") 
  let Just parIdx1 = fmap snd $ find ((==parnm1) . fst) $ zip (read parstr) [0..]
  files <- (catMaybes . map (parseFileName nm) . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  let chains = nub . fst $ unzip files
  --mapM print files
  cs <- forM chains $ \c-> do
          let fls = take 100 $ sort $ lookupMany c files
          fmap concat $ forM fls $ \fl-> do
--             fmap (map (!!parIdx) . thin 100) $ safeLoad (unparseFileName nm c fl)
            fmap (map (!!parIdx1)) $ safeLoad (unparseFileName nm c fl)

  let initvs = map (take 1) (cs::[[Double]])
  gnuplotOnScreen $ map (\c-> GnuplotBox $ AxisLabels "iteration" parnm1 $ Lines $  zip [(0::Double)..] c) cs

main2 = do
  nm:parnm1:parnm2:_ <- getArgs
  parstr <- readFile (nm++"_parnames.mcmc") 
  let Just parIdx1 = fmap snd $ find ((==parnm1) . fst) $ zip (read parstr) [0..]
  let Just parIdx2 = fmap snd $ find ((==parnm2) . fst) $ zip (read parstr) [0..]
  files <- (catMaybes . map (parseFileName nm) . filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."
  let chains = nub . fst $ unzip files
  --mapM print files
  cs <- forM chains $ \c-> do
          let fls = take 100 $ sort $ lookupMany c files
          fmap concat $ forM fls $ \fl-> do
--             fmap (map (!!parIdx) . thin 100) $ safeLoad (unparseFileName nm c fl)
            fmap (map (idx2 parIdx1 parIdx2)) $ safeLoad (unparseFileName nm c fl)

  let initvs = map (take 1) (cs::[[(Double,Double)]])
  gnuplotOnScreen $ map (\c-> GnuplotBox $ AxisLabels parnm1 parnm2 $ Lines c :+: (take 1 c)) cs
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