{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

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

uniqueIntStr = (show. hashUnique) `fmap` newUnique

data GnuplotCmd = Plot String

class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd

data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a

plotOnScreen :: PlotWithGnuplot a => a -> IO ()
plotOnScreen x = do
  Plot cmd <- getGnuplotCmd x
  let cmdLines = unlines ["set datafile missing \"NaN\"",
                          "plot "++cmd]
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  return ()


instance PlotWithGnuplot (Signal Double) where
    getGnuplotCmd s' = 
        do let (s@(Signal t1 t2 dt sf):_) = downSample 1000 [s']
           fnm <- ("/tmp/gnuplotsig"++) `fmap` uniqueIntStr
           writeSig fnm s
           return . Plot $ concat ["\"", fnm, "\" binary format=\"%float64\" using ($0*",
                                   show dt, "+", show t1, "):1 with lines"]
           where writeSig fp s@(Signal t1 t2 dt sf) = do
                   h <- openBinaryFile fp WriteMode
                   SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]
                   hClose h

instance PlotWithGnuplot [Event Double] where
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotevs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return . Plot $ concat ["\"", fnm, "\" using 1:2 with points"]
           where writeEvts fp evs = do
                   h <- openFile fp WriteMode
                   forM_ evs $ \(t,v)-> hPutStrLn h $ show t++"\t"++show v
                   hClose h

instance PlotWithGnuplot [Duration Double] where
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotdurs"++) `fmap` uniqueIntStr
           writeEvts "/tmp/gnuplotdurs" es
           return . Plot $ concat ["\"", fnm, "\" using 1:($2) with lines"]
           where writeEvts fp durs = do
                   h <- openFile fp WriteMode
                   forM_ durs $ \((t1,t2),v)-> do 
                          hPutStrLn h $ show t1++"\t"++show v
                          hPutStrLn h $ show t2++"\t"++show v
                          hPutStrLn h $ show t2++"\tNaN"
                   hClose h


infixr 2 :+:

data a :+: b = a :+: b

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (a :+: b) where
    getGnuplotCmd (xs :+: ys) = do
      Plot px <- getGnuplotCmd xs
      Plot py <- getGnuplotCmd ys                          
      return . Plot $ px++","++py



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

downSample' :: (Ord a, Bounded a, Num a) => Int -> Signal a -> Signal a
downSample' n sig@(Signal t1 t2 dt sf) =
    let npw = round $ (t2-t1)/dt
        chunkSize = floor (npw./ n)
        nChunks =  ceiling (npw ./chunkSize)
        newDt = (t2-t1)/realToFrac (nChunks*2)
        narr = listArray (0,(nChunks*2)-1 ) $concatMap chunk [0..(nChunks-1)]
        chunk i = let n1 = i*chunkSize
                      n2 = n1 + (min chunkSize (npw - i*chunkSize -1))
                      (x,y) = sigSegStat (both maxF minF) (n1,n2) sig
                      in [x,y]
     in if npw>n 
           then (Signal t1 t2 ((t2-t1)./(nChunks*2)) $ \p-> narr!p)
           else sig
        {-chunk i = let arrsec = sliceU (uVpnts w) (i*chunkSize) $ min chunkSize (npw - i*chunkSize -1)
                                           in [maximumU arrsec, minimumU arrsec]
                             in UVecWave (toU narr) ((maxt w-mint w)/2 / realToFrac nChunks) (mint w) (nChunks*2)
-}

x ./ y = realToFrac x / realToFrac y

{-
class PlotWithGnuplot a => PlotMany a where
    chopByDur :: [Duration b] -> a -> [a]

instance PlotMany [Signal Double] where
    chopByDur durs sigs = map (\dur->section sigs [dur]) durs

instance (Real a) => PlotMany [Event a] where
    chopByDur durs evs = map (\dur->during evs [dur]) durs

instance (Real a) => PlotMany [Duration a] where
    chopByDur chopDurs durs = map (\dur->sectionDur1 dur durs) chopDurs

instance (PlotMany a, PlotMany b) => PlotMany (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)
-}
{-plotManyBy :: PlotMany b => [Duration a] -> b -> [IO RPlotCmd]
plotManyBy durs pm = map getRPlotCmd $ chopByDur durs pm

instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}