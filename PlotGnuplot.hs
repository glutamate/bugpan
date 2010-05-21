{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs, ScopedTypeVariables, DeriveDataTypeable #-}

module PlotGnuplot where

--import EvalM
import System.IO
import System.Cmd
import System.Exit
import Math.Probably.FoldingStats hiding (F)
import Control.Monad
import Data.Unique
import Data.List
import Control.Monad.Trans
import TNUtils
import System.Directory
--import System.Posix.Files
import Data.Array.Unboxed
import System.Random

{- stolen from gnuplot-0.3.3 (Henning Thieleman) -}

import qualified System.Process as Proc


execGPPipe ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> IO ExitCode
execGPPipe program =
   do --putStrLn program
      (inp,_out,_err,pid) <-
         Proc.runInteractiveProcess "gnuplot" [""] Nothing Nothing
      hPutStr inp program
      --print pid
      Proc.waitForProcess pid

execGPSh ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
--   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
execGPSh program  =
   let cmd =
          "sh -c 'echo " ++ quote ( program) ++
                 " | gnuplot '"
   in  do putStrLn cmd
          system cmd

execGPPersist ::
      String {-^ The lines of the gnuplot script to be piped into gnuplot -}
--   -> [String] {-^ Options for gnuplot -}
   -> IO ()
execGPPersist cmds = do
  x <- randomRIO (0,99999999::Int)
  let fnm = "/tmp/gnuplotCmds"++show x
  writeFile fnm cmds
  system $ "gnuplot -persist "++fnm
  removeFile $ fnm

execGPTmp cmds = do
  x <- randomRIO (0,99999999::Int)
  let fnm = "/tmp/gnuplotCmds"++show x
  writeFile fnm cmds
  system $ "gnuplot "++fnm
  removeFile $ fnm


execGP = execGPTmp


semiColonConcat = concat . intersperse "; "


quote :: String -> String
quote = show

--quote s = 

{- end of theft -}

--histArr :: (Int,Int) -> [Double] -> UArray Int Double
histArr :: (Int, Int) -> [Int] -> UArray Int Double
histArr bnds is = accumArray (+) 0 bnds [( i, 1) | i<-is, inRange bnds i]

histValues :: Int -> [Double] -> [(Double,Double)]
histValues nbins vls = 
    let (hArr, lo, hi, binSize) = histList nbins vls
    in zip [lo, lo+binSize..hi] hArr

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
                           hArr = histArr (0,nbins) $ ixs
                       in (elems hArr, lo, hi, binSize)

histListFixed :: Double -> Double -> Double -> [Double] -> [Double]
histListFixed t1 t2 dt [] = take (round $ (t2-t1)/dt) $ repeat 0
histListFixed t1 t2 dt vls = let nbins = round $ (t2-t1)/dt
                                 ixs = map (\v-> floor $! (v-t1)/dt ) vls
                                 hArr = histArr (0,nbins-1) $ ixs
                             in elems hArr

                   

uniqueIntStr = (show. hashUnique) `fmap` newUnique

type GnuplotCmd = [PlotLine]

data PlotLine = PL {plotData :: String,
                    plotTitle :: String,
                    plotWith :: String,
                    cleanUp :: IO () }
              | TopLevelGnuplotCmd String String

plOnly pls = [pl | pl@(PL _ _ _ _) <- pls]
tlOnlyUnset pls = [s2 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]
tlOnly pls = [s1 | pl@(TopLevelGnuplotCmd s1 s2) <- pls]

cleanupCmds :: [GnuplotCmd] -> IO ()
cleanupCmds cmds = forM_ cmds $ \plines -> sequence_ $ map cleanUp $ plOnly plines

setWith :: String -> GnuplotCmd -> GnuplotCmd
setWith sty = map f
    where f pl@(PL _ _ _ _) = pl {plotWith = sty }
          f tlcmd = tlcmd

showPlotCmd :: GnuplotCmd -> String
showPlotCmd [] = ""
showPlotCmd [TopLevelGnuplotCmd s s2] = s ++ "\n"++ s2
showPlotCmd plines = tls++"\nplot "++(intercalate ", " $ map s $ plOnly $ plines)++"\n"++untls
    where s (PL dat tit wth _) = dat++title tit++withStr wth
          title "" = " notitle"
          title tit = " title '"++tit++"'"
          withStr "" = ""
          withStr s = " with "++s 
          tls = unlines $ tlOnly plines
          untls = unlines $ tlOnlyUnset plines

showMultiPlot :: [(Rectangle, GnuplotCmd)] -> String
showMultiPlot rpls = "set multiplot\n" ++ concatMap pl rpls ++"\nunset multiplot\n"
    where pl (r@(Rect (x0,y0) (x1,y1)), plines)=concat ["#"++show r++"\n",
                                                        "set origin ", 
                                                        show x0, ",", show y0, "\n",
                                                        "set size ", show (x1-x0),
                                                        ",", show (y1-y0), "\n",
                                                        showPlotCmd plines]
                                                      

data Rectangle = Rect (Double, Double) (Double,Double) deriving Show
unitRect = Rect (0,0) (1,1)

rectTopLeft (Rect (x1,y1) (x2,y2)) = (x1+0.01,y2-0.01) 

class PlotWithGnuplot a where
    getGnuplotCmd :: a -> IO GnuplotCmd
    getGnuplotCmd a = (snd . head) `fmap` multiPlot unitRect a

    multiPlot :: Rectangle -> a -> IO [(Rectangle, GnuplotCmd)]
    multiPlot r a = (\x->[(r, x)]) `fmap` getGnuplotCmd a

data GnuplotBox = forall a. PlotWithGnuplot a => GnuplotBox a

data Noplot = Noplot

instance PlotWithGnuplot Noplot where
    getGnuplotCmd _ = return [PL "x" "" "lines lc rgb \"white\"" (return () ),
                             TopLevelGnuplotCmd "unset border; unset tics" "set border; set tics"]

instance PlotWithGnuplot GnuplotBox where
    getGnuplotCmd (GnuplotBox x) = getGnuplotCmd x

instance PlotWithGnuplot [GnuplotBox] where
    getGnuplotCmd xs = concat `fmap` mapM getGnuplotCmd xs


gnuplotOnScreen :: PlotWithGnuplot a => a -> IO ()
gnuplotOnScreen x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot -persist /tmp/gnuplotCmds"
  --removeFile "/tmp/gnuplotCmds"
  cleanupCmds $ map snd plines
  return ()

gnuplotToPNG :: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPNG fp x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal png\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  --putStrLn cmdLines
  execGP cmdLines
  {- writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds" -}
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

  execGP cmdLines                       
  {-writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds 2>/dev/null"
  removeFile "/tmp/gnuplotCmds"-}
  cleanupCmds $ map snd plines
  return ()

gnuplotToPDF:: PlotWithGnuplot a => String -> a -> IO ()
gnuplotToPDF fp x = do
  gnuplotToPS fp x
  system $ "ps2pdf "++fp
  return ()

gnuplotToPS:: PlotWithGnuplot a => String-> a -> IO ()
gnuplotToPS fp  x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                 "set terminal postscript eps enhanced color \"Helvetica\" 8\n"++
                 "set output '"++fp++"'\n"++
                  (showMultiPlot plines)
                       
  execGP cmdLines
{-  writeFile "/tmp/gnuplotCmds" cmdLines
  system "gnuplot /tmp/gnuplotCmds"
  removeFile "/tmp/gnuplotCmds"-}
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
  execGP cmds

  forM_ nmcmds $ \(_,cmd) -> cleanupCmds $ map snd cmd
  return ()
    where plotOne (fp, plines) = "set output '"++fp++"'\n"++
                                 (showMultiPlot plines)

gnuplotManyLatex :: [String] -> [(String, GnuplotBox)] -> IO ()
gnuplotManyLatex opts nmbxs = do
  nmcmds <- forM nmbxs $ \(nm, GnuplotBox x) -> do
                      cmd <- multiPlot unitRect x
                      --print2 nm cmd
                      return (nm,cmd)
  let start = "set datafile missing \"NaN\"\n"
  let h = optVal 'h' 480 opts
  let w = optVal 'w' 640 opts
  let term = "set terminal epslatex color\n" -- size "++ show w++","++show h++" crop\n"
  let cmds = start++term ++concatMap plotOne nmcmds
  execGP cmds
  forM_ nmcmds $ \(nm,cmd) -> do
    system $ "epstopdf "++nm++".eps"
    cleanupCmds $ map snd cmd
  return ()
    where plotOne (fp, plines) = "set output '"++fp++".tex'\n"++
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

data Hplots a = Hplots [a]
data Vplots a = Vplots [a]

data PcntDiv a = Pcnt Double a

data WithColour a = WithColour String a

x % a = Pcnt x a

data SubLabel a = 
    A a | Ai a | Aii a | Aiii a
  | B a | Bi a | Bii a | Biii a
  | C a | Ci a | Cii a | Ciii a
  | D a | Di a | Dii a | Diii a
  | E a | Ei a | Eii a 
  | SubNum Int a

subLabSplit :: SubLabel a -> (String, a)
subLabSplit (A x) = ("A",x)
subLabSplit (Ai x) = ("Ai",x)
subLabSplit (Aii x) = ("Aii",x)
subLabSplit (Aiii x) = ("Aiii",x)
subLabSplit (B x) = ("B",x)
subLabSplit (Bi x) = ("Bi",x)
subLabSplit (Bii x) = ("Bii",x)
subLabSplit (Biii x) = ("Biii",x)
subLabSplit (C x) = ("C",x)
subLabSplit (Ci x) = ("Ci",x)
subLabSplit (Cii x) = ("Cii",x)
subLabSplit (Ciii x) = ("Ciii",x)
subLabSplit (D x) = ("D",x)
subLabSplit (Di x) = ("Di",x)
subLabSplit (Dii x) = ("Dii",x)
subLabSplit (Diii x) = ("Diii",x)
subLabSplit (E x) = ("E",x)
subLabSplit (SubNum n x) = (show n, x)

--newtype Lines a = Lines {unLines :: a }
--newtype Dashed a = Dashed {unDashed :: a }
newtype Boxes a = Boxes {unBoxes :: a }

data Lines a = Lines [StyleOpt] a
data Points a = Points [StyleOpt] a

data StyleOpt = LineWidth Double 
              | LineType Int
              | LineStyle Int
              | LineColor String
              | PointType Int
              | PointSize Double

styleOptsToString :: [StyleOpt] -> String
styleOptsToString = intercalate " " . map g
    where g (LineType lt) = "lt "++show lt
          g (LineWidth lt) = "lw "++show lt
          g (LineStyle lt) = "ls "++show lt
          g (LineColor lc) = "lc rgb "++show lc
          g (PointType lt) = "pt "++show lt
          g (PointSize lt) = "ps "++show lt



instance PlotWithGnuplot a => PlotWithGnuplot (Lines a) where
    multiPlot r (Lines sos x) = do
      px <- multiPlot r x
      let wstr = styleOptsToString sos
      return $ map (\(r', pls) -> (r', setWith ("lines "++wstr) pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (Points a) where
    multiPlot r (Points sos x) = do
      px <- multiPlot r x
      let wstr = styleOptsToString sos
      return $ map (\(r', pls) -> (r', setWith ("points "++wstr) pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (Boxes a) where
    multiPlot r (Boxes x) = do
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', setWith "boxes" pls)) px

data GnuplotTest = GnuplotTest

instance PlotWithGnuplot (GnuplotTest) where
    multiPlot r _ = do
      return $ [(r, [TopLevelGnuplotCmd "test" ""])]


data Margin a = Margin Double Double Double Double a
data XRange a = XRange Double Double a
data YRange a = YRange Double Double a

data XTics a = XTics [Double] a
data YTics a = YTics [Double] a

data Noaxis a = Noaxis a
              | NoXaxis a
              | NoYaxis a

setMargin (Margin b t l r _) = unlines ["set bmargin "++show b,
                                        "set lmargin "++show l,
                                        "set rmargin "++show r,
                                        "set tmargin "++show t]
                               

unsetMargin = unlines ["unset bmargin ",
                       "unset lmargin ",
                       "unset rmargin ",
                       "unset tmargin "]
instance PlotWithGnuplot a => PlotWithGnuplot (Margin a) where
    multiPlot r m@(Margin _ _ _ _ x) = do
      px <- multiPlot r x
      let setit = setMargin m
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit unsetMargin):pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (XRange a) where
    multiPlot r m@(XRange lo hi x) = do
      px <- multiPlot r x
      let setit = "set xrange ["++show lo++":"++show hi++"]\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xrange [*:*]"):pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (Noaxis a) where
    multiPlot r m@(Noaxis x) = do
      px <- multiPlot r x
      let cmd = TopLevelGnuplotCmd "unset border; unset tics" "set border; set tics"
      return $ map (\(r', pls) -> (r', cmd:pls)) px
    multiPlot r m@(NoYaxis x) = do
      px <- multiPlot r x
      let cmd = TopLevelGnuplotCmd "set border 1; set tics" "set border; set tics"
      return $ map (\(r', pls) -> (r', cmd:pls)) px

instance PlotWithGnuplot a => PlotWithGnuplot (XTics a) where
    multiPlot r m@(XTics tics x) = do
      px <- multiPlot r x
      let setit = "set xtics "++ (intercalate ", " $ map show tics) ++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xtics autofreq"):pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (YTics a) where
    multiPlot r m@(YTics tics x) = do
      px <- multiPlot r x
      let setit = "set ytics "++ (intercalate ", " $ map show tics) ++"\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set xtics autofreq"):pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (YRange a) where
    multiPlot r m@(YRange lo hi x) = do
      px <- multiPlot r x
      let setit = "set yrange ["++show lo++":"++show hi++"]\n"
      return $ map (\(r', pls) -> (r', (TopLevelGnuplotCmd setit "set yrange [*:*]"):pls)) px


lineWidth w = Lines [LineWidth w]
lineType t = Lines [LineType t]
pointSize t = Points [PointSize t]
pointType t = Points [PointType t]

data ScaleBars a = ScaleBars (Double, Double) (Double,String) (Double,String) a
                 | XScaleBar (Double, Double) (Double,String) Double a
                 | YScaleBar (Double, Double) (Double,String) Double a

data LineAt a = LineAt (Double, Double) (Double, Double) a
data ArrowAt a = ArrowAt (Double, Double) (Double, Double) a

data TextAt a = TextAt (Double, Double) String a

instance PlotWithGnuplot a => PlotWithGnuplot (TextAt a) where
    multiPlot r (TextAt (x0,y0) s x) = do
      let mklab = TopLevelGnuplotCmd ("set label "++show s++" at first "++show x0++","++show y0++" center front") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px


instance PlotWithGnuplot a => PlotWithGnuplot (LineAt a) where
    multiPlot r (LineAt (x0,y0) (x1, y1) x) = do
      let mklab = TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show y0++" to first "++show x1++","++show y1++" nohead front") 
                                     "unset arrow"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px 

instance PlotWithGnuplot a => PlotWithGnuplot (ArrowAt a) where
    multiPlot r (ArrowAt (x0,y0) (x1, y1) x) = do
      let mklab = TopLevelGnuplotCmd ("set arrow from first "++show x0++","++show y0++" to first "++show x1++","++show y1++" heads front") 
                                     "unset arrow"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px 


instance PlotWithGnuplot a => PlotWithGnuplot (ScaleBars a) where
    multiPlot r (ScaleBars p0 (xsz, xtxt) (ysz, ytxt) x) = do
      multiPlot r $ XScaleBar p0 (xsz, xtxt) (ysz/4) $ YScaleBar p0 (ysz, ytxt) (xsz/4) x     
    multiPlot r (XScaleBar (x0,y0) (xsz, xtxt) yo x) = do
      let xtxtpos = (x0+xsz/2, y0 - yo/4)
      multiPlot r $ LineAt (x0, y0) (x0+xsz, y0) 
                  $ TextAt xtxtpos xtxt x
    multiPlot r (YScaleBar (x0,y0)  (ysz, ytxt) yo x) = do
      let ytxtpos = (x0+yo, y0 + ysz/2)                                                             
      multiPlot r $ LineAt (x0, y0) (x0, y0+ysz) 
                  $ TextAt ytxtpos ytxt x



instance PlotWithGnuplot a => PlotWithGnuplot (SubLabel a) where
    multiPlot r sl = do
      let (lab, x ) = subLabSplit sl
          (xpos, ypos) = rectTopLeft r
      let mklab = TopLevelGnuplotCmd ("set label "++show lab++" at screen "++show xpos++","++show ypos++" front") 
                                     "unset label"
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklab:pls)) px

data CentreLabel = CentreLabel String

instance PlotWithGnuplot CentreLabel where
    multiPlot r (CentreLabel str)  = do      
      let mklab = TopLevelGnuplotCmd ("set label "++show str++" at graph 0.5,0.5 center front") "unset label"
      nop::[GnuplotCmd] <- fmap (map snd) $ multiPlot r Noplot

      return [(r, mklab:concat nop)]


data AxisLabels a = AxisLabels String String a
                  | XLabel String a
                  | YLabel String a

instance PlotWithGnuplot a => PlotWithGnuplot (AxisLabels a) where
    multiPlot r (AxisLabels xlab ylab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set xlabel "++show xlab) "unset xlabel", 
                    TopLevelGnuplotCmd ("set ylabel "++show ylab) "unset ylabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px
    multiPlot r (XLabel xlab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set xlabel "++show xlab) "unset xlabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px
    multiPlot r (YLabel xlab x) = do
      let mklabs = [TopLevelGnuplotCmd ("set ylabel "++show xlab) "unset ylabel"]
      px <- multiPlot r x
      return $ map (\(r', pls) -> (r', mklabs++pls)) px

data Pad a = Pad Double a
           | PadX Double a
           | PadY Double a

instance (PlotWithGnuplot a) => PlotWithGnuplot (Pad a) where
    multiPlot r (Pad p x) = multiPlot r $ PadX p $ PadY p x
    multiPlot (Rect (x0, y0) (x1,y1)) (PadX p x) = do
      let xc = (x1 - x0) / p
      px <- multiPlot (Rect (x0+xc,y0) (x1-xc, y1) ) x
      return $ px
    multiPlot (Rect (x0, y0) (x1,y1)) (PadY p x) = do
      let yc = (y1 - y0) / p
      px <- multiPlot ( Rect (x0,y0+yc) (x1, y1-yc) ) x
      return $ px

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

instance (PlotWithGnuplot a, PlotWithGnuplot b) => PlotWithGnuplot (Either a b) where
    multiPlot r (Left xs) = multiPlot r xs
    multiPlot r (Right xs) = multiPlot r xs



instance (PlotWithGnuplot a) => PlotWithGnuplot (Hplots a) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Hplots ps) = do
      let n = realToFrac $ length ps
      let xeach = (x1-x0)/n
      pls <- forM (zip ps [0..]) $ \(p,i) -> 
               multiPlot ( Rect (x0+(i*xeach),y0) (x0+((i+1)*xeach), y1) ) p
      return $ concat pls

instance (PlotWithGnuplot a) => PlotWithGnuplot (Vplots a) where
    multiPlot (Rect (x0, y0) (x1,y1)) (Vplots ps) = do
      let n = realToFrac $ length ps
      let yeach = (y1-y0)/n
      pls <- forM (zip ps [0..]) $ \(p,i) -> 
               multiPlot ( Rect (x0,y0+(i*yeach)) (x1, y0+((i+1)*yeach)) ) p
      return $ concat pls


instance PlotWithGnuplot a => PlotWithGnuplot (String, a) where
    multiPlot r (title, x) = do
      pls <- multiPlot r x
      return $ map (\(r', plines) -> (r' ,map (addTitle title) plines)) pls
      where addTitle title (PL x _ y clean) = PL x title y clean

newtype LabelConsecutively a = LabelConsecutively a

instance PlotWithGnuplot a => PlotWithGnuplot (LabelConsecutively [a]) where
    getGnuplotCmd (LabelConsecutively xs) = do
      pls::[ GnuplotCmd] <- mapM (getGnuplotCmd) xs
      return $ concatMap (\(rs,i)-> (addTitleMany (show i)) rs) $ zip pls [0..]
      where addTitle title (PL x _ y clean) = PL x title y clean
            addTitleMany :: String -> ( GnuplotCmd) -> ( GnuplotCmd)
            addTitleMany title (cmd) = ( map (addTitle title) cmd)

--tilePlots ::  PlotWithGnuplot a => Int -> [a] -> Vplots (Hplots a)
tilePlots :: Int -> [t] -> Vplots (Hplots (SubLabel (Either t Noplot)))
tilePlots n ps = let nps = (length ps) 
                     nfinal = if nps `mod` n == 0
                                 then nps
                                 else ((nps `div` n)+1)*n
                     allps = ensureLength (nfinal) Noplot ps
                 in Vplots $ map Hplots $ map (map (\(p,i) -> SubNum i p)) $ groupsOf n (zip allps [0..])

gridPlot :: [[GnuplotBox]] -> Vplots (Hplots GnuplotBox)
gridPlot plots = Vplots $ map Hplots plots

groupsOf n [] = []
groupsOf n xs = let (mine, rest) = splitAt n xs
                in mine: groupsOf n rest

ensureLength n filler xs = map Left xs++replicate (n - length xs) (Right filler)



{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}