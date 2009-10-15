{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TNUtils
import Data.List
import qualified Data.StorableVector as SV
import System.Time
import Database
--import Parse
import EvalM hiding (ListT)
--import Eval
--import Expr
--import Stages
import Query
import QueryTypes
--import Control.Monad.State.Lazy
--import HaskSyntaxUntyped
--import QueryUnsafe
--import Data.IORef
import QueryUtils
import Math.Probably.FoldingStats
--import Math.Probably.PlotR
--import QueryRun
import ValueIO
import Numbers
--import Tests.Asserts
import System.Environment
import Data.Ord
import Control.Monad 
import Data.Unique
import System.Cmd
import PlotGnuplot
import QueryPlots
--import RandomSources
import System.Random.Mersenne

--1. AVERAGE WAVEFORMS CI1-SETI and CI1-FLEXOR
--2. for every ci1 spike which is nearest : seti or flexor?


--D0

main = do
  args <- getArgs
  dispatch args

manyDurs :: Int -> Double -> Double -> [Duration ()]
manyDurs n tsep tmax= let durs = replicate n ((0,tmax),())
                      in map (\(oneDur,i)->shift (i*tsep) oneDur)  $ zip durs [0..]
 
--extensor max 0.030 s
--flexor max 0.025 s

burst :: Double -> [Event a] -> [Duration ()]
burst tmax es = burst' Nothing tmax es
burst' :: Maybe Double -> Double -> [Event a] -> [Duration ()]
burst' (Nothing) tmax es@(e:[]) = []
burst' (Just tbstart) tmax ((t,v):[]) = [((tbstart, t), ())]
burst' (Just tbstart) tmax ((t1,v1):res@((t2,v2):es)) 
    | dist t1 t2 > tmax = ((tbstart, t1), ()) : burst' (Nothing) tmax res
    | otherwise = burst' (Just tbstart) tmax res
burst' (Nothing) tmax ((t1,v1):res@((t2,v2):es)) | dist t1 t2 < tmax = burst' (Just t1) tmax res
                                                 | otherwise = burst' (Nothing) tmax res

totalDuration :: [Duration a] -> Double
totalDuration = sum . map ((uncurry $flip (-)) . fst)


dispatch ("import":_) = do
  importAnimalIn "AM"
  importAnimalIn "AR"
  importAnimalIn "BB"
  importAnimalIn "AV"
  importAnimalIn "BR"
  importAnimalIn "BK"
  return ()

dispatch ("analyse":sess:_) = do
  inSessionNamed sess $ do
    scratch <- durations "scratch" ()
    --flexSpikes <- events "flex1Spikes" ()
    flexSpikes <- sortBy (comparing fst) `fmap` events "flex1Spikes" ()
    ci1Spikes <- sortBy (comparing fst) `fmap` events "ci1Spikes" ()
    seti <- sortBy (comparing fst) `fmap` events "setiSpikes" ()
    feti <- sortBy (comparing fst) `fmap` events "fetiSpikes" ()
    flexor1 <- signalsDirect "flexor1"
    ci1 <- signalsDirect "ci1"
    depol <- durations "depol" (1::Double)
    cyc1 <- durations "firstCycle" ()
    --pois1 <- io $ manyPoisson 1000 10 1 20
    --pois2 <- io $ manyPoisson 1000 10 1 20
    --let poisDurs = manyDurs 1000 10 1
    --let ivls = map getTag $ intervalsOver scratch $ spikes
    --rHisto 100 $ crossCorrelateOver scratch ci1Spikes flexSpikes
    let bz = 0.01
    let scratchd0 = during (isZero//depol) scratch
    let cc evs evs2 =   let ctrl =crossCorrelateOverControl scratchd0 evs2 evs
                            s1 = normSigToArea (histSigBZ bz (crossCorrelateOver scratchd0 evs2 evs))
                            s2 = normSigToArea $ histSigBZ bz (ctrl)
                        in [limitSig (-0.6) 0.6 $ s1-s2]
    let ccflex =  cc flexSpikes ci1Spikes
    let ccseti = cc seti ci1Spikes
    let ccfeti = cc feti ci1Spikes
    let ccflexseti = cc flexSpikes seti
    
    
    
    --io $ gnuplotToPDF ("cc_ci_seti_"++sess++".ps") $ ("cc ci1Spikes seti "++sess, 
    --                          Histo 100 $  (>(-1)) // (<2) //  crossCorrelateOver scratch ci1Spikes seti)
    {-io $ gnuplotToPDF ("cc_"++sess++".ps") $ (("ci1 seti "++sess,  ccseti) :||: ("ci1 feti "++sess,  ccfeti))
                                                :==:
                                                (("ci1 flexor "++sess, ccflex) :||: ("seti flexor "++sess, ccflexseti))-}
    --io $ gnuplotToPDF ("cc_ci_feti_"++sess++".ps") $ ("cc ci1 feti "++sess,  ccfeti)
    --io $ gnuplotToPDF ("cc_ci_flex_"++sess++".ps") $ ("cc ci1 flexor "++sess, ccflex)
    --io $ gnuplotToPDF ("cc_seti_flex"++sess++".ps") $ ("cc seti flexor "++sess, ccflexseti)
    --io $ gnuplotToPDF ("cc_ci_feti_"++sess++".ps") $ ("cc ci1Spikes feti "++sess, 
     --                         Histo 100 $  (>(-1)) // (<2) //  crossCorrelateOver scratch ci1Spikes feti)
{-    io $ gnuplotToPS "afterCI.ps" $ ("0-50 ms after ciSpike", 
                            averageSigs $ limitSigs' (-0.010) (0.010) $ around (during (fadeOut 0.05 ci1Spikes) flexSpikes) flexor1)-}
    --io $ gnuplotOnScreen $ ("all CI spikes", 
                        --    averageSigs $ limitSigs' (-0.010) (0.010) $ take 100 $ around (ci1Spikes) ci1)
    openReplies
    --ask $ plot $ ccflexseti
    --ask $ plot $ Histo 100 $ intervalsOver poisDurs $ pois1
    --ctrl <- crossCorrelateOverControl scratch ci1Spikes seti
    --ask $ plot [normSigToArea $ histSigBZ bz ctrl]
    --ask $ plot [normSigToArea (histSigBZ bz (crossCorrelateOver scratch ci1Spikes seti))]
    --ask $ plot [normSigToArea (histSigBZ bz (crossCorrelateOver scratch ci1Spikes seti)) - (normSigToArea $ histSigBZ bz ctrl)]
    --io $ print $ (isSorted ci1Spikes
    --io $ print $ isSorted seti
    --io $ print $ map (round . fst) ci1Spikes
    let nearSeti = (<0.5)//(nearestToEach ci1Spikes seti)
    let nearFlex = (<0.5)//(nearestToEach ci1Spikes flexSpikes)
    let clos = map closestSpike $ zip (nearSeti) (nearFlex)
    let burstSeti = burst 0.03 $ during scratchd0 seti
    let burstFlex = burst 0.025 $ during scratchd0 flexSpikes

    let duringSeti = (realToFrac . sumTags $ countDuring burstSeti ci1Spikes )  / (totalDuration burstSeti)
    let duringFlex = (realToFrac . sumTags $ countDuring burstFlex ci1Spikes )  / (totalDuration burstFlex)


    --io $ putStrLn $ sess++" ci1-seti distances "++(show .headTag $ (meanSEMF `runStatsOn` nearSeti))
    --io $ putStrLn $ sess++" ci1-flexor distances "++(show .headTag $ (meanSEMF `runStatsOn` nearFlex))
    io $ putStrLn $ sess++"\t"++(show duringSeti)++"\t"++(show duringFlex)
    --io $ putStrLn $ sess++" ci1-flexor freq "++(show duringFlex)
    --io $ putStrLn $ sess++" 0: seti closest, 1: flex closest -> "++(show $ meanF `runStat` clos)
    --io $ putStrLn ""
    --io $ gnuplotOnScreen $ [histSigBZ bz nearSeti] :+: [histSigBZ bz nearFlex]
    return () 

headTag = snd . head

closestSpike ((_,s0),(_,s1)) = if s0<s1 then 0 else 1

--rHisto :: MonadIO m => [Double] -> m ()
 
brenda :: [Signal Double] -> [GnuplotBox]
brenda sigs = [GnuplotBox $ Brenda $ averageSigs sigs]
 
--foo = during

isZero x = x>(-0.5) && x<0.5 

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
           (angles, spikes, ephys, footfram, cycfram) <- io $ impLBR $ dir ./ fnm 
           let scratchLength = (realToFrac $ length $ head ephys)*dt
           let t0 = filesSecDiff finfo firstInfo
           let onedurD x = [((t0, t0+scratchLength),realToFrac x)]::[((Double,Double),Double)]
           let onedur x = [((t0, t0+scratchLength), x)]
           storeAsAppend "flexor1" [listToSig dt t0 $ ephys!!0]
           storeAsAppend "flexor2" [listToSig dt t0 $ ephys!!1]
           storeAsAppend "extensor" [listToSig dt t0 $ ephys!!2]
           storeAsAppend "n5a" [listToSig dt t0 $ ephys!!3]
           storeAsAppend "ci1" [listToSig dt t0 $ ephys!!4]
           storeAsAppend "cxTr" [listToSig dtAngles t0 $ angles!!0]
           storeAsAppend "feTi" [listToSig dtAngles t0 $ angles!!1]
           storeAsAppend "tiTa" [listToSig dtAngles t0 $ angles!!2]
           storeAsAppend "thCx" [listToSig dtAngles t0 $ angles!!3]

           storeAsAppend "flex1Spikes" $ spikesToEvents dt t0 1 $ spikes !!0
           storeAsAppend "flex2Spikes" $ spikesToEvents dt t0 1 $ spikes !!1
           storeAsAppend "setiSpikes" $ spikesToEvents dt t0 1 $ spikes !!2
           storeAsAppend "fetiSpikes" $ spikesToEvents dt t0 2 $ spikes !!2
           --storeAsAppend "n5aSpikes" $ spikesToEvents dt t0 1 $ spikes !!3
           storeAsAppend "ci1Spikes" $ spikesToEvents dt t0 1 $ spikes !!3

           storeAsAppend "scratch" $ onedur ()
           storeAsAppend "load" $ onedurD $ load finfo
           storeAsAppend "depol" $ onedurD $ depol finfo
           storeAsAppend "cycles" $ onedurD $ cycles finfo 
           storeAsAppend "isPosterior" $ onedur $ isPosterior finfo
           storeAsAppend "repNum" $ onedurD $ repnum finfo
           storeAsAppend "tStart" $ [(t0,())]
           storeAsAppend "tStop" $ [(t0+scratchLength,())]
           storeAsAppend "firstCycle" $ [((t0+(realToFrac footfram)*dtAngles, 
                                           t0+(realToFrac cycfram)*dtAngles),())]


spikesToEvents :: Double -> Double -> Int -> [Int] -> [Event ()]
spikesToEvents dt t0 ident ints = let ts = map ((+t0) . snd) $ filter ((==ident) . fst) $ zip ints $ map (*dt) [0..]
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
  

  return (take nangles angles, take nspikes spikes, take nmyst myst, n3, n4)

readMatrix :: Read a => [String] -> ([[a]], Int, [String])
readMatrix rest = 
    let mystInfo = splitBy ' ' $ head rest
        mystNumLn ::Int = read $ mystInfo!!0
        mystNumCol ::Int = read $ mystInfo!!1
        (mystLines, rest') = splitAt mystNumLn $ drop 1 rest
                             
        myst = transpose $ for mystLines $ \ln -> map read $ splitBy ' ' ln
    in (myst,mystNumCol,rest')
