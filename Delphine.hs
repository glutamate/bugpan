{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TNUtils
import Data.List
import qualified Data.StorableVector as SV
import System.Time
import Database
import Parse
import EvalM hiding (ListT)
import Eval
import Expr
--import Stages
import Query
import QueryTypes
--import Control.Monad.State.Lazy
import HaskSyntaxUntyped
--import QueryUnsafe
--import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
--import QueryRun
import ValueIO
import Numbers
--import Tests.Asserts
import PlotGnuplot
import System.Environment
import Data.Ord
import Control.Monad 
import Data.Unique
import System.Cmd
import PlotGnuplot

main = do
  args <- getArgs
  dispatch args

dispatch ("import":_) = do
  importAnimalIn "AM"
  importAnimalIn "AR"
  return ()

dispatch ("analyse":sess:_) = do
  inSessionNamed sess $ do
    scratch <- durations "scratch" ()
    flexSpikes <- events "flex1Spikes" ()
    extSpikes <- events "extSpikes" ()
    ci1Spikes <- events "ci1Spikes" ()

    --let ivls = map getTag $ intervalsOver scratch $ spikes
    --rHisto 100 $ crossCorrelateOver scratch ci1Spikes flexSpikes
    rHistoScreen 100 $ crossCorrelateOver scratch ci1Spikes extSpikes
    
    return ()

--rHisto :: MonadIO m => [Double] -> m ()



importAnimalIn dir = do
  allfiles <- getDirContents dir
  let files = filter (".lbr" `isSuffixOf`) allfiles
  let finfos = sortBy (comparing snd) $ zip files $ map parseFileName files
  let firstInfo = snd $ head finfos
  let sessNm = anim $ snd $ head finfos
  let dt = 1.7398/8699
  let dtAngles = 0.05
  mapM_ (putStrLn . fst) finfos
  inNewSessionWith sessNm (fileT0 firstInfo) $ do
       forM_ finfos $ \(fnm,finfo) -> do
           (angles, spikes, ephys) <- io $ impLBR $ dir ./ fnm 
           let scratchLength = (realToFrac $ length $ head ephys)*dt
           let t0 = filesSecDiff finfo firstInfo
           let onedur x = [((t0, t0+scratchLength),x)] 
           storeAs "flexor1" [listToSig dt t0 $ ephys!!0]
           storeAs "flexor2" [listToSig dt t0 $ ephys!!1]
           storeAs "extensor" [listToSig dt t0 $ ephys!!2]
           storeAs "n5a" [listToSig dt t0 $ ephys!!3]
           storeAs "ci1" [listToSig dt t0 $ ephys!!4]
           storeAs "cxTr" [listToSig dtAngles t0 $ angles!!0]
           storeAs "feTi" [listToSig dtAngles t0 $ angles!!1]
           storeAs "tiTa" [listToSig dtAngles t0 $ angles!!2]
           storeAs "thCx" [listToSig dtAngles t0 $ angles!!3]

           storeAs "flex1Spikes" $ spikesToEvents dt t0 $ spikes !!0
           storeAs "flex2Spikes" $ spikesToEvents dt t0 $ spikes !!1
           storeAs "extSpikes" $ spikesToEvents dt t0 $ spikes !!2
           storeAs "ci1Spikes" $ spikesToEvents dt t0 $ spikes !!3

           storeAs "scratch" $ onedur ()
           storeAs "load" $ onedur $ load finfo
           storeAs "depol" $ onedur $ depol finfo
           storeAs "cycles" $ onedur $ cycles finfo 
           storeAs "isPosterior" $ onedur $ isPosterior finfo
           storeAs "repNum" $ onedur $ repnum finfo
           storeAs "tStart" $ [(t0,())]
           storeAs "tStop" $ [(t0+scratchLength,())]
        


spikesToEvents :: Double -> Double -> [Int] -> [Event ()]
spikesToEvents dt t0 ints = let ts = map ((+t0) . snd) $ filter ((>0) . fst) $ zip ints $ map (*dt) [0..]
                            in zip ts $ repeat ()

months = enumFromTo January December

fileT0 :: FileNameInfo -> ClockTime
fileT0 (FNI _ _ _ _ d mo h mi s _ _) = toClockTime $ CalendarTime 2008 (months!!(mo-1)) d h mi s 0 
                                                                  undefined undefined undefined 0 undefined

filesSecDiff :: FileNameInfo -> FileNameInfo -> Double
filesSecDiff (FNI _ _ _ _ d1 mo1 h1 mi1 s1 _ _) (FNI _ _ _ _ d mo h mi s _ _) = realToFrac $ (s1-s)+(mi1-mi)*60+(h1-h)*3600

data FileNameInfo = FNI {
      anim :: String,
      load :: Int,
      depol :: Int,
      cycles :: Int,
      day:: Int,
      month :: Int,
      hour :: Int,
      minute:: Int,
      second :: Int,
      repnum:: Int,
      isPosterior :: Bool
    } deriving (Show, Eq)

secsSinceMidnight (FNI _ _ _ _ d1 mo1 h1 mi1 s1 _ _) = s1+60*mi1+3600*h1

--sort on creation time
instance Ord FileNameInfo where 
    compare fn1 fn2 = compare (secsSinceMidnight fn1) (secsSinceMidnight fn2)


parseFileName :: String -> FileNameInfo
parseFileName n = let (a:l:d:c:da:m:h:min:rest) = splitBy '-' n
                      (ss:rn:loc:_) = splitBy '_' $ concat rest
                      rd s = read $ drop 1 s
                  in FNI a (rd l) (rd d) (rd c) (read m) (read da) (read h) (read min) (read ss) (read rn) (loc=="post")
-- "AM-L0-D0-C01-04-28-18-29-00_04_post_comb.lbr"
impLBR fnm = do
  lns <- lines `fmap` readFile fnm 
  let n1::Int = read $ lns!!0
  let n2::Int = read $ lns!!1
  --let ang = splitBy ' ' $ lns!!2
  --let angleNumLines::Int = read $ ang!!0
  --let angleCols::Int = read $ ang!!1
  --let (angleLines, rest) = splitAt angleNumLines $ drop 3 lns
  let (angles:: [[Double]], nangles, rest) = readMatrix $ drop 2 lns -- transpose $ for angleLines $ \ln -> map read $ splitBy ' ' ln
  
  --print $ take 5 $ angles !! 0
 
  let (myst:: [[Double]], nmyst, rest' ) = readMatrix rest -- transpose $ for mystLines $ \ln -> map read $ splitBy ' ' ln

  --print $ take 5 $ myst !! 0
  --print $ take 5 $ myst !! 4

  let (spikes:: [[Int]], nspikes, rest'' ) = readMatrix rest'

  let n3::Int = read $ rest''!!0
  let n4::Int = read $ rest''!!1

  --print $ parseFileName fnm
  

  return (take nangles angles, take nspikes spikes, take nmyst myst)

listToSig dt t1 lst = let arr = SV.pack lst
                          t2 = (realToFrac $ length lst) *dt +t1
                      in Signal t1 t2 dt $ \pt->arr `SV.index` pt

readMatrix :: Read a => [String] -> ([[a]], Int, [String])
readMatrix rest = 
    let mystInfo = splitBy ' ' $ head rest
        mystNumLn ::Int = read $ mystInfo!!0
        mystNumCol ::Int = read $ mystInfo!!1
        (mystLines, rest') = splitAt mystNumLn $ drop 1 rest
                             
        myst = transpose $ for mystLines $ \ln -> map read $ splitBy ' ' ln
    in (myst,mystNumCol,rest')
