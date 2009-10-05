{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs #-}

module PlotGnuplot where

--import EvalM
import System.IO
import System.Cmd
import Math.Probably.FoldingStats
import Control.Monad
import Data.Unique
import Data.List
import Control.Monad.Trans
import qualified Data.StorableVector as SV
import TNUtils
import System.Directory
import System.Posix.Files
import Data.Array.Unboxed



--histArr :: (Int,Int) -> [Double] -> UArray Int Double
histArr :: (Int, Int) -> [Int] -> UArray Int Double
histArr bnds is = accumArray (+) 0 bnds [( i, 1) | i<-is, inRange bnds i]

histList :: Int -> [Double] -> ([Double] , Double, Double, Double)
histList _ [] = ([], 0, 0, 1)
histList nbins vls = let lo = foldl1' min vls
                         hi = foldl1' max vls
                         binSize = (hi-lo)/(realToFrac nbins+1)
                         ixs = map (\v-> floor $! (v-lo)/binSize ) vls
                         hArr = histArr (0,nbins-1) $ ixs
                     in (elems hArr, lo, hi, binSize)

histListBZ :: Double -> [Double] -> ([Double] , Double, Double, Double)
histListBZ _ [] = ([], 0, 0, 1)
histListBZ bz vls    = let lo = foldl1' min vls
                           hi = foldl1' max vls
                           binSize = bz
                           nbins = round $ (hi-lo)/bz
                           ixs = map (\v-> floor $! (v-lo)/binSize ) vls
                           hArr = histArr (0,nbins-1) $ ixs
                       in (elems hArr, lo, hi, binSize)
                   

uniqueIntStr = (show. hashUnique) `fmap` newUnique

type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String,
                    cleanUp :: IO () } -- deriving Show

cleanupCmds :: [GnuplotCmd] -> IO ()
cleanupCmds cmds = forM_ cmds $ \plines -> sequence_ $ map cleanUp plines

setWith :: String -> GnuplotCmd -> GnuplotCmd
setWith sty = map (\pl-> pl {plotWith = sty })

showPlotCmd :: GnuplotCmd -> String
showPlotCmd [] = ""
showPlotCmd plines = "plot "++(intercalate ", " $ map s plines)++"\n"
    where s (PL dat tit wth _) = dat++title tit++" with "++wth
          title "" = " notitle"
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

data Noplot = Noplot

instance PlotWithGnuplot Noplot where
    getGnuplotCmd _ = return []

instance PlotWithGnuplot GnuplotBox where
    getGnuplotCmd (GnuplotBox x) = getGnuplotCmd x


gnuplotOnScreen :: PlotWithGnuplot a => a -> IO ()
gnuplotOnScreen x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
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
  removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
  return ()

gnuplotToSparklinePNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToSparklinePNG fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png size 100,50 crop\n"++
                 "unset xtics\n"++
                 "unset ytics\n"++
                 "set border 0\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds 2>/dev/null"
  removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
  return ()

gnuplotToPDF:: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPDF fp x = do
  gnuplotToPS fp x
  system $ "ps2pdf "++fp
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
  removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
  return ()


gnuplotMany :: [String] -> [(String, GnuplotBox)] -> IO ()
gnuplotMany opts nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- multiPlot unitRect x
                      --print2 nm cmd
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"
  let h = optVal 'h' 480 opts
  let w = optVal 'w' 640 opts
  let term = "set terminal png size "++ show w++","++show h++" crop\n"
  let cmds = start++term ++concatMap plotOne nmcmds
  writeFile "/tmp/gnuplotCmds" cmds
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds"
  forM_ nmcmds $ \(_,cmd) -> cleanupCmds $ map snd cmd

  return ()
    where plotOne (fp, plines) = "set output '"++fp++"'\n"++
                                 (showMultiPlot plines)
  

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


newtype Lines a = Lines {unLines :: a }
newtype Dashed a = Dashed {unDashed :: a }
newtype Boxes a = Boxes {unBoxes :: a }

instance PlotWithGnuplot a => PlotWithGnuplot (Boxes a) where
    multiPlot r (Boxes x) = do
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', setWith "boxes" pls)) px
    getGnuplotCmd (Boxes x) = do
      px <- getGnuplotCmd x
      return $ setWith "boxes" px

instance PlotWithGnuplot a => PlotWithGnuplot (Dashed a) where
    multiPlot r (Dashed x) = do
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', setWith "lines ls 0" pls)) px
    getGnuplotCmd (Dashed x) = do
      px <- getGnuplotCmd x
      return $ setWith "lines" px

instance PlotWithGnuplot a => PlotWithGnuplot (Lines a) where
    multiPlot r (Lines x) = do
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', setWith "lines" pls)) px
    getGnuplotCmd (Lines x) = do
      px <- getGnuplotCmd x
      return $ setWith "lines" px



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
      where addTitle title (PL x _ y clean) = PL x title y clean





{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}