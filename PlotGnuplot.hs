{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module PlotGnuplot where

import EvalM
import qualified Data.StorableVector as SV
import System.IO
import System.Cmd
import QueryTypes
--import Array
import Math.Probably.FoldingStats
import Control.Monad
import Data.Unique
import Data.List

uniqueIntStr = (show. hashUnique) `fmap` newUnique

type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String }

showPlotCmd :: GnuplotCmd -> String
showPlotCmd plines = "plot "++(intercalate ", " $ map s plines)++"\n"
    where s (PL dat tit wth) = dat++title tit++" with "++wth
          title "" = ""
          title tit = " title '"++tit++"'"

data Rectangle = Rect (Double, Double) (Double,Double)


class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd
    multiPlot :: a -> Rectangle -> IO GnuplotCmd
    multiPlot a _ = getGnuplotCmd a

data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a



instance QueryResult [GnuplotBox] where
    qFilterSuccess [] = False
    qFilterSuccess _ = True
    qReply gpbxs = do 
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
  plines <- getGnuplotCmd x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showPlotCmd plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  return ()

gnuplotToPNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPNG fp x = do
  plines <- getGnuplotCmd x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png\n"++
                 "set output '"++fp++"'\n"++
                  (showPlotCmd plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  return ()

gnuplotMany :: [(String, GnuplotBox)] -> IO ()
gnuplotMany nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- getGnuplotCmd x
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"++
                 "set terminal png\n"
  let cmds = start++concatMap plotOne nmcmds
  writeFile "/tmp/gnuplotCmds" cmds
  system "gnuplot /tmp/gnuplotCmds"
  return ()
    where plotOne (fp, plines) = "set output '"++fp++"'\n"++
                                 (showPlotCmd plines)
  
instance PlotWithGnuplot [Signal Double] where
    getGnuplotCmd ss = forM (downSample 1000 ss) $ \s@(Signal t1 t2 dt sf) -> do
           fnm <- ("/tmp/gnuplotsig"++) `fmap` uniqueIntStr
           writeSig fnm s
           return $ PL (concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                    show dt, "+", show t1, "):1"]) "" "lines"
           where writeSig fp s@(Signal t1 t2 dt sf) = do
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


infixr 3 :+:
infixr 2 :|:
infixr 1 :--:

data a :+: b = a :+: b

data a :|: b = a :|: b

data a :--: b = a :--: b

data PcntDiv a = Pcnt Int a

data WithColour a = WithColour String a

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :+: b) where
    getGnuplotCmd (xs :+: ys) = do
      px <- getGnuplotCmd xs
      py <- getGnuplotCmd ys                          
      return $ px++py

instance PlotWithGnuplot a => PlotWithGnuplot (String, a) where
    getGnuplotCmd (title, x) = do
      plines <- getGnuplotCmd x
      return $ map (addTitle title) plines
      where addTitle title (PL x _ y) = PL x title y

gnuPlotSig :: Signal Double -> IO ()
gnuPlotSig s@(Signal t1 t2 dt sf) = do
  h <- openBinaryFile "/tmp/gnuplotsig" WriteMode
  SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]
  hClose h
  let cmds = unlines [concat ["plot \"/tmp/gnuplotsig\" binary format=\"%float64\" using ($0*",
                              show dt, "+", show t1, "):1 with lines"]]
  writeFile "/tmp/gnuplotCmds" cmds
  system "gnuplot /tmp/gnuplotCmds -persist"
  return ()

--echo 'plot "/tmp/gnuplotsig" binary format="%float64" using ($0*0.1+3.3):1 with lines' | gnuplot -persist

downSample n = map (downSample' (n `div` 2))

--downSample' :: (Ord a, Bounded a, Num a, Storable a) => Int -> Signal a -> Signal a
downSample' :: Int -> Signal Double -> Signal Double
downSample' n sig@(Signal t1 t2 dt sf) =
    let npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./ n)
        nChunks =  ceiling (npw ./chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = SV.pack $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)./(nChunks*2)) $ \p-> narr `SV.index` p)
           else sig
        {-chunk i = let arrsec = sliceU (uVpnts w) (i*chunkSize) $ min chunkSize (npw - i*chunkSize -1)
                                           in [maximumU arrsec, minimumU arrsec]
                             in UVecWave (toU narr) ((maxt w-mint w)/2 / realToFrac nChunks) (mint w) (nChunks*2)
-}

x ./ y = realToFrac x / realToFrac y




instance (ChopByDur a, ChopByDur b) =>  ChopByDur (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)

instance ChopByDur a =>  ChopByDur (String,a) where
    chopByDur durs (nm, x) = map ((,) nm) (chopByDur durs x) 


{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}