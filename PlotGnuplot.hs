{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs #-}

module PlotGnuplot where

import EvalM
import qualified Data.StorableVector as SV
import System.IO
import System.Cmd
import QueryTypes
import Array
import Math.Probably.FoldingStats
import Control.Monad
import Data.Unique
import Data.List
import Control.Monad.Trans

data Histo where -- GADT bec i don't know syntax for double existential (no longer needed)
    Histo :: Tagged t => Int -> [t Double] -> Histo 
    AsPdf :: String -> Histo -> Histo

instance QueryResult Histo where
    qFilterSuccess _ = True
    qReply (Histo nbins vls) _ = rHisto nbins vls "x11(width=10,height=7)"
    qReply (AsPdf nm (Histo nbins vls)) _ = rHisto nbins vls $ "pdf(\""++nm++"\")"


instance PlotWithGnuplot Histo where
    getGnuplotCmd (Histo n vls) = do
            fnm <- ("/tmp/gnuplothist"++) `fmap` uniqueIntStr
            writeHist fnm n $ map getTag vls
            return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "boxes"]
        where writeHist fp n vls = do
                   let (counts, lo, hi, binSize) = histList n vls
                   --print n
                   --print (counts, lo, hi, binSize)
                   h <- openFile fp WriteMode
                   let dat = zip [lo, lo+binSize..hi] counts
                   forM_  dat $ \(x,y)-> hPutStrLn h $ show x++"\t"++show y
                   hClose h

histArr :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
histArr bnds is = accumArray (+) 0 bnds [(i, 1) | i<-is, inRange bnds i]

histList :: (RealFrac a) => Int -> [a] -> ([a] , a, a, a)
histList nbins vls = let lo = foldl1 min vls
                         hi = foldl1 max vls
                         binSize = (hi-lo)/(realToFrac nbins+1)
                         ixs = map (\v-> floor $ (v-lo)/binSize ) vls
                         hArr = histArr (0,nbins-1) $ ixs
                     in (elems hArr, lo, hi, binSize)
                   


rHisto nbins vls outdev = liftIO $ do
               fnm <- (("/tmp/rhisto"++) . show. hashUnique) `fmap` newUnique
               writeFile fnm .  unlines $ map (show . getTStart) vls
               fnmCmd <- (("/tmp/rhisto"++) . show. hashUnique) `fmap` newUnique
               writeFile fnmCmd $ unlines [outdev,
                                           "xs <- scan(\""++ fnm++"\")",
                                           "hist(xs,"++show nbins++")",
                                           "z<-locator(1)",
                                           "q()"]
               system $ "R --vanilla --slave < "++fnmCmd
               return ""

rHistoScreen n vs = rHisto n vs "x11(width=10,height=7)"

uniqueIntStr = (show. hashUnique) `fmap` newUnique

type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String } deriving Show

showPlotCmd :: GnuplotCmd -> String
showPlotCmd plines = "plot "++(intercalate ", " $ map s plines)++"\n"
    where s (PL dat tit wth) = dat++title tit++" with "++wth
          title "" = ""
          title tit = " title '"++tit++"'"

showMultiPlot :: [(Rectangle, GnuplotCmd)] -> String
showMultiPlot rpls = "set multiplot\n" ++ concatMap pl rpls ++"unset multiplot\n"
    where pl (r@(Rect (x0,y0) (x1,y1)), plines)=concat ["#"++show r++"\n",
                                                        "set origin ", 
                                                        show x0, ",", show y0, "\n",
                                                        "set size ", show (x1-x0),
                                                        ",", show (y1-y0), "\n",
                                                        showPlotCmd plines]
                                                      

data Rectangle = Rect (Double, Double) (Double,Double) deriving Show
unitRect = Rect (0,0) (1,1)

class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd
    getGnuplotCmd a = (snd . head) `fmap` multiPlot unitRect a

    multiPlot :: Rectangle -> a -> IO [(Rectangle, GnuplotCmd)]
    multiPlot r a = (\x->[(r, x)]) `fmap` getGnuplotCmd a

data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a



instance QueryResult [GnuplotBox] where
    qFilterSuccess [] = False
    qFilterSuccess _ = True
    qReply gpbxs _ = do 
      u <- (show. hashUnique) `fmap` newUnique
      let htmlFile  ="/var/bugpan/www/plots"++u++".html" 
      h <- openFile (htmlFile) WriteMode
      fnms <- forM gpbxs .  const $ do fnm <- (++".png") `fmap` uniqueIntStr
                                       hPutStrLn h $ concat ["<img src=\"", fnm, "\" /><p />"]
                                       return $ "/var/bugpan/www/"++fnm
      gnuplotMany $ zip fnms gpbxs
      hClose h
      --plotPlotCmd plot
      --system $ "gnome-open file://"++ htmlFile
      return $ "file://"++ htmlFile

plot :: PlotWithGnuplot a => a -> [GnuplotBox]
plot x = [GnuplotBox x]

plotManySigs :: PlotWithGnuplot [a] => [a] -> [GnuplotBox]
plotManySigs ss = map (\s->GnuplotBox [s]) ss

plotManyBy :: (PlotWithGnuplot b, ChopByDur b) => [Duration a] -> b -> [GnuplotBox]
plotManyBy durs pm = map GnuplotBox $ chopByDur durs pm

scatter :: Tagged t => [t (a,b)] -> [(a,b)]
scatter = map getTag -- uses Event PLotWithGnuplot instance :-)
    

gnuplotOnScreen :: PlotWithGnuplot a => a -> IO ()
gnuplotOnScreen x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  return ()

gnuplotToPNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPNG fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  return ()

gnuplotToPS:: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPS fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal postscript\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  return ()


gnuplotMany :: [(String, GnuplotBox)] -> IO ()
gnuplotMany nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- multiPlot unitRect x
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"++
                 "set terminal png\n"
  let cmds = start++concatMap plotOne nmcmds
  writeFile "/tmp/gnuplotCmds" cmds
  system "gnuplot /tmp/gnuplotCmds"
  return ()
    where plotOne (fp, plines) = "set output '"++fp++"'\n"++
                                 (showMultiPlot plines)
  
instance PlotWithGnuplot [Signal Double] where
    getGnuplotCmd ss = forM (downSample 1000 ss) $ \s@(Signal t1 t2 dt sf) -> do
           fnm <- ("/tmp/gnuplotsig"++) `fmap` uniqueIntStr
           writeSig fnm s
           return $ PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"]) "" "lines"
          
writeSig fp s@(Signal t1 t2 dt sf) = do
  h <- openBinaryFile fp WriteMode
  SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]
  hClose h

instance PlotWithGnuplot [Event Double] where
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotevs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "points"]
        where writeEvts fp evs = do
                   h <- openFile fp WriteMode
                   forM_ evs $ \(t,v)-> hPutStrLn h $ show t++"\t"++show v
                   hClose h

instance PlotWithGnuplot [Duration Double] where
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotdurs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:($2)"]) "" "lines"]
           where writeEvts fp durs = do
                   h <- openFile fp WriteMode
                   forM_ durs $ \((t1,t2),v)-> do 
                          hPutStrLn h $ show t1++"\t"++show v
                          hPutStrLn h $ show t2++"\t"++show v
                          hPutStrLn h $ show t2++"\tNaN"
                   hClose h

data Brenda = Brenda [Signal Double]

instance PlotWithGnuplot Brenda where
    getGnuplotCmd (Brenda l@(avg:plusSEM:minusSEM:[])) = 
        forM (downSample 1000 l) $ \s@(Signal t1 t2 dt sf) -> do
           fnm <- ("/tmp/gnuplotsig"++) `fmap` uniqueIntStr
           writeSig fnm s
           return $ PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"]) "" "lines"
           where writeSigArea fp s1@(Signal t1 t2 dt sf) s2@(Signal t1' t2' dt' sf') = do
                   h <- openBinaryFile fp WriteMode
                   SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]
                   hClose h

infixl 4 %
infixr 3 :+:
infixr 2 :|:
infixr 1 :--:

data a :+: b = a :+: b

data a :||: b = a :||: b
data a :|: b = PcntDiv a :|: PcntDiv b

data a :--: b = PcntDiv a :--: PcntDiv b
data a :==: b =  a :==: b

data PcntDiv a = Pcnt Double a

data WithColour a = WithColour String a

x % a = Pcnt x a


instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :||: b) where
    multiPlot r (xs :||: ys) = multiPlot r (50% xs :|: 50% ys)

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :==: b) where
    multiPlot r (xs :==: ys) = multiPlot r (50% xs :--: 50% ys)


instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot ( a :|: b) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Pcnt pcp p :|: Pcnt pcq q) = do
      let xsep = x0+(pcp/(pcp+pcq))*(x1-x0)
      px <- multiPlot ( Rect (x0,y0) (xsep, y1) ) p
      py <- multiPlot ( Rect (xsep,y0) (x1, y1) ) q
      return $ px++py 

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot ( a :--: b) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Pcnt pcp p :--: Pcnt pcq q) = do
      let ysep = y0+(pcp/(pcp+pcq))*(y1-y0)
      px <- multiPlot ( Rect (x0,y0) (x1, ysep) ) q
      py <- multiPlot ( Rect (x0, ysep) (x1, y1) ) p
      return $ py++px 

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :+: b) where
    multiPlot r (xs :+: ys) = do
      px <- getGnuplotCmd xs
      py <- getGnuplotCmd ys                          
      return $ [(r,px++py)]

instance PlotWithGnuplot a => PlotWithGnuplot (String, a) where
    multiPlot r (title, x) = do
      pls <- multiPlot r x
      return $ map (\(r', plines) -> (r' ,map (addTitle title) plines)) pls
      where addTitle title (PL x _ y) = PL x title y



downSample n = map (downSample' (n `div` 2))

--downSample' :: (Ord a, Bounded a, Num a, Storable a) => Int -> Signal a -> Signal a
downSample' :: Int -> Signal Double -> Signal Double
downSample' n sig@(Signal t1 t2 dt sf) =
    let x ./. y = realToFrac x / realToFrac y
        npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./. n)
        nChunks =  ceiling (npw ./. chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = SV.pack $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)./.(nChunks*2)) $ \p-> narr `SV.index` p)
           else sig






instance (ChopByDur a, ChopByDur b) =>  ChopByDur (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)

instance ChopByDur a =>  ChopByDur (String,a) where
    chopByDur durs (nm, x) = map ((,) nm) (chopByDur durs x) 


{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}