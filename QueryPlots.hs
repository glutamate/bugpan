{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, ExistentialQuantification #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, GADTs #-}

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
import TNUtils
import System.Directory
import System.Posix.Files
import PlotGnuplot
import Foreign.Storable

data Histo where -- GADT bec i don't know syntax for double existential (no longer needed)
    Histo :: Int -> [(a,Double)] -> Histo 
    AsPdf :: String -> Histo -> Histo



instance PlotWithGnuplot Histo where
    getGnuplotCmd (Histo n vls) = do
            fnm <- ("/tmp/gnuplothist"++) `fmap` uniqueIntStr
            writeHist fnm n $ map snd vls
            return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "boxes"]
        where writeHist fp n vls = do
                   let (counts, lo, hi, binSize) = histList n vls
                   --print n
                   --print (counts, lo, hi, binSize)
                   h <- openFile fp WriteMode
                   let dat = zip [lo, lo+binSize..hi] counts
                   forM_  dat $ \(x,y)-> hPutStrLn h $ show x++"\t"++show y
                   hClose h



instance QueryResult [GnuplotBox] where
    qFilterSuccess [] = False
    qFilterSuccess _ = True
    qReply gpbxs opts = do 
      let resdir = optStr 'd' "somewhere" opts
      unlessM (doesDirectoryExist $ "/var/bugpan/www/"++resdir) $ do
                       createDirectoryIfMissing False $ "/var/bugpan/www/"++resdir
                       system $ "chmod 777 /var/bugpan/www/"++resdir
                       return ()
      --createDirectoryIfMissing False $ "/var/bugpan/www/"++resdir
--      setFileMode ("/var/bugpan/www/"++resdir) 777
      --system $ "chmod 777 /var/bugpan/www/"++resdir
      --p <- getPermissions ("/var/bugpan/www/"++resdir) 
      --setPermissions ("/var/bugpan/www/"++resdir) $ p { writable = True }
      u <- (show. hashUnique) `fmap` newUnique
      let htmlFile  ="/var/bugpan/www/"./resdir./("plots"++u++".html" )
      h <- openFile (htmlFile) WriteMode
      fnms <- forM gpbxs .  const $ do fnm <- (++".png") `fmap` uniqueIntStr
                                       hPutStrLn h $ concat ["<img src=\"", "/var/bugpan/www/"./resdir./fnm, "\" style=\"float: left\"/>"]
                                       return $ "/var/bugpan/www/"./resdir./fnm
      gnuplotMany opts $ zip fnms gpbxs
      hClose h
      --plotPlotCmd plot
      --system $ "gnome-open file://"++ htmlFile
      system $ "chmod 777 /var/bugpan/www/"++resdir./"/* 2>/dev/null"

      return $ "file://"++ htmlFile

plot :: PlotWithGnuplot a => a -> [GnuplotBox]
plot x = [GnuplotBox x]

plotManySigs :: PlotWithGnuplot [a] => [a] -> [GnuplotBox]
plotManySigs ss = map (\s->GnuplotBox [s]) ss

plotManyBy :: (PlotWithGnuplot b, ChopByDur b) => [Duration a] -> b -> [GnuplotBox]
plotManyBy durs pm = map GnuplotBox $ chopByDur durs pm

scatter :: Tagged t => [t (a,b)] -> [(a,b)]
scatter = map getTag -- uses Event PLotWithGnuplot instance :-)
    

  
instance PlotWithGnuplot [Signal Double] where
    getGnuplotCmd [] = return []
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
    getGnuplotCmd [] = return []
    getGnuplotCmd es = 
        do fnm <- ("/tmp/gnuplotevs"++) `fmap` uniqueIntStr
           writeEvts fnm es
           return [PL (concat ["\"", fnm, "\" using 1:2"]) "" "points"]
        where writeEvts fp evs = do
                   h <- openFile fp WriteMode
                   forM_ evs $ \(t,v)-> hPutStrLn h $ show t++"\t"++show v
                   hClose h

instance PlotWithGnuplot [Duration Double] where
    getGnuplotCmd [] = return []
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





instance (ChopByDur a, ChopByDur b) =>  ChopByDur (a :+: b) where
    chopByDur durs (x :+: y) = zipWith (:+:) (chopByDur durs x) (chopByDur durs y)

instance ChopByDur a =>  ChopByDur (String,a) where
    chopByDur durs (nm, x) = map ((,) nm) (chopByDur durs x) 

webSpark :: [String] -> Bool -> [V] -> IO String
webSpark opts isHist xs= do 
  let vls = if isHist 
               then let (vls', _, _, _) = histList 50 $ map unsafeReify xs in zip [(1::Double)..] vls' 
               else zip [(1::Double)..] (map (idDouble . unsafeReify) xs)
  print vls
  u <- (show. hashUnique) `fmap` newUnique
  let resdir = optStr 'd' "somewhere" opts
  unlessM (doesDirectoryExist $ "/var/bugpan/www/"++resdir) $ do
                       createDirectoryIfMissing False $ "/var/bugpan/www/"++resdir
                       system $ "chmod 777 /var/bugpan/www/"++resdir
                       return ()
  --p <- getPermissions 
  --setPermissions ("/var/bugpan/www/"++resdir) $ p { writable = True }
  let fnm = "/var/bugpan/www/"./resdir./"spark"++u++".png" 
  gnuplotToSparklinePNG fnm vls
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

instance (Show a, Reify a) => QueryResult [Event a] where
    qReply [xs] opts | grid opts = if Unit == (pack . snd $ xs)
                                      then return . show . fst  $ xs
                                      else return . show . snd  $ xs
                     | otherwise = return $ show xs
    qReply [] opts = return "[]"
    qReply xs opts | grid opts = case (pack . snd . head $ xs) of
                                   Unit   -> webSpark opts True $ map (pack . fst) xs -- histo of intervals. instead: dot for occ?
                                   NumV _ -> webSpark opts True $ map (pack . snd) xs
                                   _ -> return $ show xs
                   | otherwise = return $ show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True

instance (Show a, Reify a) => QueryResult [Duration a] where
    qReply [xs] opts | grid opts = if Unit == (pack . snd $ xs)
                                      then return . (\(t1,t2)->show t1++" -> "++show t2) . fst  $ xs
                                      else return . show . snd  $ xs
                     | otherwise = return $ show xs
    qReply [] opts = return "[]"
    qReply xs opts | grid opts = case (pack . snd . head $ xs) of --instead: line for each, height extent?
                                   Unit   -> webSpark opts True $ map (pack . uncurry (-) . fst) xs --histo of time extents.
                                   NumV _ -> webSpark opts True $ map (pack . snd) xs    
                                   _ -> return $ unlines $ map show xs
                   | otherwise = return $ unlines $ map show xs
    --qReply xs opts = return $ unlines $ map show xs
    qFilterSuccess [] = False
    qFilterSuccess _ = True

{-instance Num a => PlotWithR (Hist a) where
    getRPlotCmd (Histogram tgs) = 
        plotHisto $ map getTag tgs

-}