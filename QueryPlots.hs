{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, OverlappingInstances #-}

module QueryPlots where

import EvalM
import System.IO
import System.Cmd
import QueryTypes
import Math.Probably.FoldingStats
import Control.Monad
import Data.Unique
import Data.List
import Control.Monad.Trans
import qualified Data.StorableVector as SV
import Control.Monad.State.Lazy
import TNUtils
import System.Directory
--import System.Posix.Files
import PlotGnuplot
import Foreign.Storable
import Data.Maybe
import QueryUtils
import Text.Printf
import Database
import NewSignal
import PrettyPrint
import System.Environment
import Math.Probably.Sampler

data Histo where -- GADT bec i don't know syntax for double existential (no longer needed)
    Histo :: Int -> [(a,Double)] -> Histo 
    AsPdf :: String -> Histo -> Histo


getQueryIdentifier = do 
  --args <- getArgs          
  allArgs <- getArgs -- (fileNm:rest)
  let (opts, _) = partition beginsWithHyphen allArgs
  return $ optStr 'd' "somewhere" opts


instance PlotWithGnuplot Histo where
    getGnuplotCmd (Histo _ []) = return []
    getGnuplotCmd (Histo n vls) = do
            q<- getQueryIdentifier
            fnm <- (("/tmp/gnuplothist"++q)++) `fmap` uniqueIntStr
            writeHist fnm n $ map snd vls
            return [PL (concat ["\"", fnm, "\" using 1:2"]) 
                       "" 
                       "boxes" 
                       (removeFile fnm)]
        where writeHist fp n vls = do
                   let (counts, lo, hi, binSize) = histList n vls
                   --print n
                   --print (counts, lo, hi, binSize)
                   h <- openFile fp WriteMode
                   let dat = zip [lo, lo+binSize..hi] counts
                   forM_  dat $ \(x,y)-> hPutStrLn h $ show x++"\t"++show y
                   hClose h


data FunSeg = FunSeg Double Double (Double -> Double )

instance PlotWithGnuplot FunSeg where
    getGnuplotCmd (FunSeg t1 t2 f) = let dx = (t2-t1)/1000 
                                         xs = map (\p-> p*dx+t1) [0..999] in 
                                     getGnuplotCmd $ Lines [LineStyle 0] $ zip xs $ map f xs


instance QueryResult [GnuplotBox] where
    qFilterSuccess [] = False
    qFilterSuccess _ = True
    qReply gpbxs opts = if "-litlatex" `elem` opts 
                           then queryManyGnuBoxLit gpbxs opts
                           else queryManyGnuBox gpbxs opts
      

queryManyGnuBox gpbxs opts = do 
      let resdir = optStr 'd' "somewhere" opts
      unlessM (doesDirectoryExist $ "/var/bugpan/www/"++resdir) $ do
                       createDirectoryIfMissing False $ "/var/bugpan/www/"++resdir
                       system $ "chmod 777 /var/bugpan/www/"++resdir
                       return ()
      u <- (show. hashUnique) `fmap` newUnique
      let htmlFile  ="/var/bugpan/www/"./resdir./("plots"++u++".html" )
      h <- openFile (htmlFile) WriteMode
      fnms <- forM gpbxs .  const $ do fnm <- (++".png") `fmap` uniqueIntStr
                                       hPutStrLn h $ concat ["<img src=\"", "/var/bugpan/www/"./resdir./fnm, "\" style=\"float: left\"/>"]
                                       return $ "/var/bugpan/www/"./resdir./fnm
      gnuplotMany opts $ zip fnms gpbxs
      hClose h
      --plotPlotCmd plot
      
      system $ "chmod 777 /var/bugpan/www/"++resdir./"/* 2>/dev/null"
      when ("-o" `elem` opts) $ do 
        system $ "gnome-open file://"++ htmlFile
        return ()
      return $ "file://"++ htmlFile


queryManyGnuBoxLit gpbxs opts = do 
      u <- (show. hashUnique) `fmap` newUnique
      fnms <- forM gpbxs .  const $ do fnm <- ("latpic"++) `fmap` uniqueIntStr
                                       --putStrLn $ "\\input{"++fnm++"}"
                                       return $ fnm
      gnuplotManyLatex opts $ zip fnms gpbxs 
      return $ unlines $ map (\fn->"\\includegraphics{"++fn++"}") fnms



plot :: PlotWithGnuplot a => a -> [GnuplotBox]
plot x = [GnuplotBox x]

plotManySigs :: PlotWithGnuplot [a] => [a] -> [GnuplotBox]
plotManySigs ss = map (\s->GnuplotBox [s]) ss

plotManyBy :: (PlotWithGnuplot b, ChopByDur b) => [Duration a] -> b -> [GnuplotBox]
plotManyBy durs pm = map GnuplotBox $ chopByDur durs pm

scatter :: Tagged t => [t (a,b)] -> [(a,b)]
scatter = map getTag -- uses Event PLotWithGnuplot instance :-)
    

data DownSamplePts = DownTo Int [Signal Double]

instance PlotWithGnuplot (DownSamplePts) where
    getGnuplotCmd (DownTo _ []) = return []
    getGnuplotCmd (DownTo downs ss) = do
      q<- getQueryIdentifier
      forM (downSample downs ss) $ \s@(Signal t1 t2 dt sf _) -> do
           fnm <- (("/tmp/gnuplotsig"++q)++) `fmap` uniqueIntStr
           writeSig fnm $ forceSigEq s
           return $ PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"]) 
                       "" -- (show t1++"->"++show t2) 
                       "lines"
                       (removeFile fnm)

instance PlotWithGnuplot [Signal Double] where
    getGnuplotCmd ss = getGnuplotCmd $ DownTo 1000 ss
          
writeSig :: String -> Signal Double -> IO ()
writeSig fp s@(Signal t1 t2 dt sf Eq) = do
  h <- openBinaryFile fp WriteMode
  SV.hPut h sf
  hClose h

instance Num a => PlotWithGnuplot [Event a] where
    getGnuplotCmd [] = return []
    getGnuplotCmd es = 
        do q<- getQueryIdentifier
           fnm <- (("/tmp/gnuplotevs"++q)++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "points" (removeFile fnm)]
        where writeEvts fp evs = do
                   h <- openFile fp WriteMode
                   forM_ evs $ \(t,v)-> hPutStrLn h $ show t++"\t"++show v
                   hClose h

instance Num a => PlotWithGnuplot [Duration a] where
    getGnuplotCmd [] = return []
    getGnuplotCmd es = 
        do q<- getQueryIdentifier
           fnm <- (("/tmp/gnuplotdurs"++q)++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:($2)"]) "" "lines" (removeFile fnm)]
           where writeEvts fp durs = do
                   h <- openFile fp WriteMode
                   forM_ durs $ \((t1,t2),v)-> do 
                          hPutStrLn h $ show t1++"\t"++show v
                          hPutStrLn h $ show t2++"\t"++show v
                          hPutStrLn h $ show t2++"\tNaN"
                   hClose h

data Brenda = Brenda [Signal Double]

instance PlotWithGnuplot Brenda where
    getGnuplotCmd (Brenda l@(avg:plusSEM:minusSEM:[])) = do
        q<- getQueryIdentifier        
        forM (downSample 1000 l) $ \s@(Signal t1 t2 dt sf _) -> do
           fnm <- (("/tmp/gnuplotsig"++q)++) `fmap` uniqueIntStr
           writeSig fnm s
           return $ PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"]) "" "lines" (removeFile fnm)
           where writeSigArea fp s1@(Signal t1 t2 dt sf _) s2@(Signal t1' t2' dt' sf' _) = do
                   h <- openBinaryFile fp WriteMode
                   fail "not implemented: plot brenda"
                   --SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]
                   hClose h





instance (ChopByDur a, ChopByDur b) =>  ChopByDur (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)

instance ChopByDur a =>  ChopByDur (String,a) where
    chopByDur durs (nm, x) = map ((,) nm) (chopByDur durs x) 

instance ChopByDur DownSamplePts where
    chopByDur durs (DownTo n sigs) = map (DownTo n) $ chopByDur durs sigs 


webSpark :: [String] -> Bool -> [V] -> IO String
webSpark opts isHist xs= do 
  let vls = if isHist 
               then let (vls', _, _, _) = histList 50 $ map unsafeReify xs in zip [(1::Double)..] vls' 
               else zip [(1::Double)..] (map (idDouble . unsafeReify) xs)
  --print vls
  u <- (show. hashUnique) `fmap` newUnique
  let resdir = optStr 'd' "somewhere" opts
  unlessM (doesDirectoryExist $ "/var/bugpan/www/"++resdir) $ do
                       createDirectoryIfMissing False $ "/var/bugpan/www/"++resdir
                       system $ "chmod 777 /var/bugpan/www/"++resdir
                       return ()
  --p <- getPermissions 
  --setPermissions ("/var/bugpan/www/"++resdir) $ p { writable = True }
  let fnm = "/var/bugpan/www/"./resdir./"spark"++u++".png" 
  if isHist 
     then gnuplotToSparklinePNG fnm (Boxes vls)
     else gnuplotToSparklinePNG fnm (Lines [] vls)
--  make so vls >>= savePngFile fnm
  system $ "chmod 777 /var/bugpan/www/"++resdir./"/* 2>/dev/null"
  return $ "<img src=\""++fnm++"\" />"

idDouble :: Double -> Double
idDouble = id


instance (Ord a, Bounded a, Num a, Storable a, Reify a) => QueryResult [Signal a] where
    qReply [sig] opts | grid opts = let sig' = downSample' 100 sig
                                        vls = sigToList sig'
                                        minPt = negate $ foldl1 (min)  vls
                                    in webSpark opts False $ map (pack . (+minPt)) $ vls
    qReply [] opts = return "[]"
    qReply xs opts = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True

showPrec n x = show $ (realToFrac $ round (x*10**n))/(10**n)
minWidth n s | length s < n = replicate (n-length s) ' ' ++ s
             | otherwise = s
                 

class Show a => AccuShow a where
    accushow :: a -> String
    accushow = show

instance AccuShow Int
instance AccuShow Integer
instance AccuShow [Char]
instance AccuShow Bool
instance AccuShow ()

instance AccuShow Float where
    accushow = printf "%.3g"

instance AccuShow Double where
    accushow = printf "%.3g"

instance (AccuShow a, AccuShow b) => AccuShow (a,b) where
    accushow (x,y) = "("++accushow x++","++accushow y++")"

instance AccuShow a => AccuShow [a] where
    accushow xs = "["++intercalate "," (map accushow xs)++"]"


instance QueryResult [(Int, (Double, Double))] where
    qFilterSuccess = not . null
    qReply xs opts = do
      return $ unlines $ map printIt xs
        where printIt (n, (mu, sd)) = (minWidth 2 $ show n)++", "++showPrec 2 mu++", "++showPrec 2 sd
                                      

replyTagged :: (Show (t a), Reify a, Tagged t) => [t a] -> String -> IO String 
replyTagged xs nm = do
  let ty = "type = "++nm++" "++(ppType $ typeOfReified $ getTag $ head xs)
  return $ unlines $ ty:map show xs


instance (Show a, Reify a, AccuShow a) => QueryResult [Event a] where
    qReply [xs] opts | grid opts = if Unit == (pack . snd $ xs)
                                      then return . accushow . fst  $ xs
                                      else return . accushow . snd  $ xs
                     | otherwise = replyTagged [xs] "Events"
    qReply [] opts = return "[]"
    qReply xs opts | grid opts = case (pack . snd . head $ xs) of
                                   Unit   -> webSpark opts True $ map (pack . fst) xs -- histo of intervals. instead: dot for occ?
                                   NumV _ -> webSpark opts True $ map (pack . snd) xs
                                   _ -> return $ show xs
                   | otherwise = replyTagged xs "Events"
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance (Show a, Reify a,AccuShow a) => QueryResult [Duration a] where
    qReply [xs] opts | grid opts = if Unit == (pack . snd $ xs)
                                      then return . (\(t1,t2)->accushow t1++" -> "++accushow t2) . fst  $ xs
                                      else return . accushow . snd  $ xs
                     | otherwise = replyTagged [xs] "Durations"
    qReply [] opts = return "[]"
    qReply xs opts | grid opts = case (pack . snd . head $ xs) of --instead: line for each, height extent?
                                   Unit   -> webSpark opts True $ map (pack . uncurry (-) . fst) xs --histo of time extents.
                                   NumV _ -> webSpark opts True $ map (pack . snd) xs    
                                   _ -> return $ unlines $ map show xs
                   | otherwise = replyTagged xs "Durations"
    --qReply xs opts = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True

{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}


--SPikeDetector ONLY
askPics :: (QueryResult a, MonadIO m) => a -> StateT QState m [String]
askPics qx = do
  x <- qResThroughSession qx
  args <- shArgs `fmap` get
  qos <- liftIO $ qReply x args
  --liftIO $ putStrLn str
  --let str = concat [s | QString s <- qos ]
  conts <- liftIO $ readFile $ drop 7 qos
  return $ extractImages conts

askPicsIO :: QueryResult a => a -> IO [String]
askPicsIO x = do
  qos <- qReply x []
  --liftIO $ putStrLn str
  --let str = concat [s | QString s <- qos ]
  conts <- readFile $ drop 7 qos
  return $ extractImages conts

unitList x = [x]

extractImages :: String -> [String]
extractImages txt = catMaybes $ map f $ lines txt
    where f s | "<img src=\"" `isPrefixOf` s = Just $ takeWhile (/='"') $ drop 10 s
              | otherwise = Nothing

plotClusterMeans :: [Event Int] -> [Signal Double] -> LabelConsecutively [[Signal Double]]
plotClusterMeans evs sigs = let idxs = sort $ nubTags evs 
                                avg i = milliSecs $ head $ averageSigs $ 
                                        unjitter $ 
                                        limitSigs' (-0.001) 0.001 $ 
                                        around ((==i)//evs) $ sigs
                            in LabelConsecutively $ map (unitList . avg) idxs


plotClusters :: [Event Int] -> [Signal Double] -> Vplots (Hplots (SubLabel (Either [Signal Double] Noplot))) 
plotClusters evs sigs = let idxs = sort $ nubTags evs 
                            allSigs i = milliSecs $ limitSigs' (-0.001) 0.001 $ 
                                    around ((==i)//evs) $ sigs
                        in tilePlots 3 $ map allSigs idxs

tagElem :: (Eq a, Tagged t) => [a] -> [t a] -> [t a]
tagElem acceptTags tagged = (`elem` acceptTags)//tagged

mkIntListDur:: [Int] -> [Duration [Int]]
mkIntListDur xs = [((minBound,maxBound),xs)]

acceptSpikes :: [Duration [Int]] -> [Event Int] -> [Event ()]
acceptSpikes ((_,okTags):_) clusts = tag () $ tagElem okTags clusts

milliSecs :: Shiftable t => t -> t
milliSecs = rebaseTime 1000

nubTags = nub . map getTag 

data ChopxAxis a = ChopxAxis Double [Duration ()] a

instance (PlotWithGnuplot a, ChopByDur a, Shiftable a) 
                      => PlotWithGnuplot (ChopxAxis a) where 
    multiPlot r m@(ChopxAxis ivl durs x) = do
      let objs = chopByDur durs x
      let f (shft, lastt2) (t1,t2) = (t1 - lastt2+shft, t2 +ivl)
          firstt = snd $ fst $ head durs
      let shifts = drop 1 $ map (negate . fst) $ scanl f (0,firstt) $ map fst durs
      --liftIO $ print $ map fst durs
      --liftIO $ print shifts
      pxs <- mapM getGnuplotCmd $ map (uncurry shift) $ zip shifts objs
      return [(r, concat pxs)]

data CatScat = CatScat [(String, [Double])]

instance PlotWithGnuplot CatScat where
    getGnuplotCmd (CatScat lst) = do
      let f (idx, xs) = zip (repeat idx) xs
      let vls = map f $ zip [(0::Double)..] $ map snd lst      
      let ncats = length lst 
      let lbls = zip [(0::Int)..] $ map fst lst
      pls <- getGnuplotCmd $ jitterxs 0.1 $ concat vls
      let cmds (n,lbl) = show lbl++" "++show n
      let s1 = "set xtics ("++(intercalate "," $ map cmds lbls)++")"
      let cmd = TopLevelGnuplotCmd s1 "unset xtics"
      return (cmd:pls)

data SamHist = SamHist Int Int (Sampler Double)

instance PlotWithGnuplot SamHist where
    getGnuplotCmd (SamHist nsam nbins sam) = do
      vls <- take nsam `fmap` runSamplerIO sam
      getGnuplotCmd $ Histo nbins $ zip (repeat ()) vls




--file:///var/bugpan/www/01a11b43ac06eef1e89e/plots1.html