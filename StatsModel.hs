{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fvia-c -optc-O3 #-}
module StatsModel where

--import HaskSyntaxUntyped
--import Expr
--import EvalM
--import Numbers
import System.Environment
import Data.Char
import Data.List
import TNUtils
import Query 
import Control.Monad
import Math.Probably.Sampler
import Math.Probably.StochFun
import Math.Probably.MCMC
import qualified Math.Probably.PDF as P
import QueryTypes
import Math.Probably.FoldingStats
import PlotGnuplot
import QueryPlots
import QueryUtils hiding (groupBy)
import Database
--import Data.Array.Vector 
import qualified Data.Vector.Unboxed as U
import Data.Binary
import GHC.Conc
import qualified Control.Exception as C
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import Foreign.Storable
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import System.Directory
import Text.Regex.Posix
import Data.Maybe
import Control.Monad.Trans
import ValueIO
import Control.Applicative
--import qualified Data.Map as Map


parseFileName :: String -> String -> Maybe (Int, Int)
parseFileName setnm filenm = 
    let mat = "^"++setnm++"_chain(.+)_file(.+).mcmc$" in -- [[mat, filenm]]
    case filenm =~ mat of 
      [[all, cnum, fnum]]-> liftM2 (,) (safeRead cnum) (safeRead fnum)
      _ -> Nothing

unparseFileName :: String -> Int -> Int -> String
unparseFileName setnm cnum fnum = setnm++"_chain"++show cnum++"_file"++show fnum++".mcmc"


getFiles nm = (catMaybes .
               map (parseFileName nm) . 
               filter (nm `isPrefixOf`)) `fmap` getDirectoryContents "."



safeLoad :: String -> IO [[Double]]
safeLoad file = C.catch (fmap (map SV.unpack) $ loadVectors file)
                        (\e->return $ const [] (e::C.SomeException))

loadChain :: String -> String -> Int -> (Int,Int) -> IO [Double]
loadChain nm parnm cnum (flo, fhi) = do
  parstr <- readFile (nm++"_parnames.mcmc") 
  let Just parIdx = fmap snd $ find ((==parnm) . fst) $ zip (read parstr) [0..]
  xs <- forM [flo..fhi] $ \fnum-> do 
          let file =(nm++"_chain"++show cnum++"_file"++show fnum++".mcmc")
--          putStr $ file++" "
          ifM (doesFileExist file ) 
              (fmap (map (!!parIdx)) $safeLoad file)             
              (return [])
  return $ concat xs

takeRandomly :: Int -> [a] -> IO [a]
takeRandomly n xs = do
  fmap (take n) $ runSamplerIO $ oneOf xs

loadChainMap :: MonadIO m => String -> Int -> (Int,Int) -> Int -> Int -> m [(String,[Double])]
loadChainMap nm cnum (flo, fhi) takeN dropN = do
  parstrs <- liftIO $ fmap read $ readFile (nm++"_parnames.mcmc") 
  --let Just parIdx = fmap snd $ find ((==parnm) . fst) $ zip (read parstr) [0..]
  xs <- liftIO $ forM [flo..fhi] $ \fnum-> do 
          let file =(nm++"_chain"++show cnum++"_file"++show fnum++".mcmc")
--          putStr $ file++" "
          ifM (doesFileExist file ) 
              (safeLoad file)             
              (return [])
  xss <- liftIO$ runSamplerIO $ mapM (nOf takeN . drop dropN) $ transpose $ concat xs
  return $ zip parstrs $ head xss
      where nOf n lst = sequence $ replicate n $ oneOf lst
            joinMap k mv = fmap ((,) k) mv 
  
newtype Samples a = Samples {unSamples :: [a] } deriving (Eq, Ord, Show)

pickSamples :: [(String,[a])] -> IO [(String,a)]
pickSamples = mapM $ \(s,xs)-> fmap (((,) s) . head) $ runSamplerIO $ oneOf xs

instance Functor Samples where
    fmap f = Samples . map f . unSamples

instance Applicative Samples where
    pure x = Samples $ repeat x
    (Samples fs) <*> (Samples xs) = Samples $ zipWith ($) fs xs

samOp2 op (Samples xs) (Samples ys) = Samples $ zipWith op xs ys

thinSamples n = Samples . thin n . unSamples

samMean (Samples xs) = runStat meanF xs

samplesGaussian (Samples xs)= 
                let (mu,sd) = runStat meanSDF xs
                    gaussian = P.gauss mu sd
                    dx = 6*sd/100
                in  for [0..99] $ \i-> let x = i*dx+(mu-3*sd) in (x, gaussian $ i*dx+(mu-3*sd))

instance Num a => Num (Samples a) where
    (+) = samOp2 (+)
    (*) = samOp2 (*)
    (-) = samOp2 (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger = Samples . repeat . fromInteger

instance Fractional a => Fractional (Samples a) where
    (/) = samOp2 (/)
    fromRational = Samples . repeat . fromRational    

onlyKeys :: Eq k => [k] -> [(k,v)] -> [(k,v)]
onlyKeys ks = filter ((`elem` ks) . fst)

mapScat :: [String] -> [Samples Double] -> CatScat
mapScat nms sams = CatScat $ zip nms $ map unSamples sams

instance PlotWithGnuplot (Samples Double) where
   getGnuplotCmd (Samples xs) = getGnuplotCmd $ Histo 50 $ xs

mapSingly2 :: Eq k => k -> (v->v->a) -> k -> [(k,[v])] -> [a]
mapSingly2 k1 op k2 mp = 
    let xs = fromJust $ lookup k1 mp
        ys = fromJust $ lookup k2 mp
    in  zipWith op xs ys

m !!! k = fromJust $ lookup k m  

mapSinglyMany :: Eq k => [(k, v->v->a, k, knew)] -> [(k,[v])] -> [(knew,[a])]
mapSinglyMany ks mp = for ks $ \(k1, op, k2, knew) -> (knew, mapSingly2 k1 op k2 mp)


writeInChunksK :: (Show a, Binary b) => String -> Int ->  (a->b) -> [a] -> IO ()
writeInChunksK = writeInChunks' 0
    where writeInChunks' _ _ _  k [] = return ()
          writeInChunks' counter fnm chsize k xs = do
            let (out, rest) = splitAt chsize xs
            h <- openBinaryFile (fnm++"_file"++(show counter)++".mcmc") WriteMode           
            writeBinary h $ length out
            xlast <- mapMretLast (writeBinary h) $ map k out
            writeBinary h xlast
            hClose h
            writeInChunks' (counter+1) fnm chsize k rest

--writeInChunks :: (Binary a) => String -> Int ->   [Int] -> IO ()
writeInChunks ::  String -> Int ->   [[Double]] -> IO ()
writeInChunks = writeInChunks' 0
    where writeInChunks' _ _ _  [] = return ()
          writeInChunks' counter fnm chsize xs = do
            let (out, rest) = splitAt chsize xs
            saveVectors (fnm++"_file"++(show counter)++".mcmc") $ map SV.pack out
            writeInChunks' (counter+1) fnm chsize rest

saveVectors :: String -> [SV.Vector Double] -> IO ()
saveVectors nm svs = do
    h <- openBinaryFile nm WriteMode
    putInt h (length svs)
    mapM (\sv-> putInt h (SV.length sv) >> SV.hPut h sv) svs
    hClose h 

putInt h = L.hPut h . encode
getInt h = idInt `fmap` binGet h 8

loadVectors :: String -> IO [SV.Vector Double]
loadVectors nm = withBinaryFile nm ReadMode $ \h->  do 
    nvecs <- getInt h
    forM [1..nvecs] $ \i -> do nelems <- getInt h
                               SV.hGet h nelems

mapMretLast :: Monad m => (a-> m b) -> [a] -> m a
mapMretLast f [x] = f x >> return x
mapMretLast f (x:xs) = f x >> mapMretLast f xs

bigSigma :: Num b =>  [a] -> (a->b)-> b
bigSigma xs f = sum $ map f xs


manyLikeH :: (ChopByDur obs,Shiftable obs) => 
             [Duration [Int]] -> 
            ([Int] -> obs -> P.PDF theta1) -> 
            (theta -> theta1) -> 
            (obs -> P.PDF theta)
manyLikeH durs lh1 thetaf obs  = 
    let z = zip (chopAndReset durs obs) durs
    in \theta-> let theta' = thetaf theta in
                sum $ map (\(obs, (_,ints)) -> lh1 ints obs theta') $ z

within :: [Duration [Int]] -> [Duration [Int]] -> [Duration [Int]]
within short long = concatMap f $ relabelWithin long short 
    where f d@(t1t2, sints) = case sectionDur1 d long of 
                                  []-> []
                                  (_,lints):_ -> [(t1t2, lints++sints)]

distinct :: [Duration a] -> [Duration [Int]]
distinct durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]


relabelWithin :: [Duration a] -> [Duration [Int]] -> [Duration [Int]]
relabelWithin long short = concatMap (f . (:[])) long
    where f onelong = (:[]) <$$> (tagMany [0..] $ during onelong short)

--distinctWithin :: [Duration a] -> [Duration [Int]]
--distinctWithin durs = map (\((t1t2,_),n)->(t1t2,[n])) $ zip durs [0..]

--non-heirarchical
manyLikeOver :: (ChopByDur obs,Shiftable obs) => [Duration a] -> (theta -> P.PDF obs) -> (obs -> P.PDF theta)
manyLikeOver durs lh1 = \obs-> \theta-> sum $ map (lh1 theta) $ chopAndReset durs obs 



{-instance ChopByDur (UArr Double) where
    chopByDur durs arr = map (\((t1,t2),_)->filterU (\t->t>t1 && t<t2 ) arr) durs-}

instance ChopByDur (U.Vector Double) where
    chopByDur durs arr = map (\((t1,t2),_)->U.filter (\t->t>t1 && t<t2 ) arr) durs

instance ChopByDur (SV.Vector Double) where
    chopByDur durs arr = map (\((t1,t2),_)->SV.filter (\t->t>t1 && t<t2 ) arr) durs


{-instance Shiftable (UArr Double) where
    shift ts = mapU (+ts)
    rebaseTime = undefined-}

instance Shiftable (U.Vector Double) where
    shift ts = U.map (+ts)
    rebaseTime = undefined


instance Shiftable (SV.Vector Double) where
    shift ts = SV.map (+ts)
    rebaseTime = undefined


jumpProbBy :: (a -> a -> Bool) -> [a] -> Double
jumpProbBy eqf xs = jPB xs 0 0 
    where jPB (x:xs@(y:_)) js tots | eqf x y = jPB xs js (tots+1)
                                   | otherwise = jPB xs (js+1) (tots+1)
          jPB _ jumps total = realToFrac jumps / realToFrac total

inPar :: Int -> (Int -> IO ()) -> IO ()
inPar 0 ma = return ()
inPar 1 ma = ma 0
inPar n ma = do
  tids <- forM [0..n-1] $ \i -> forkIO (ma i)
  loop tids
    where loop tds = do
               threadDelay $ 200000
               tss <- mapM threadStatus tds
               if all (okStatus) tss
                  then return ()
                  else loop tds
          okStatus ThreadFinished = True
          okStatus ThreadDied = True
          okStatus _ = False

  

instance Shiftable Double where
    shift = (+)
    rebaseTime = undefined



lastn n xs = let len = length xs 
             in if n > len
                   then xs
                   else drop (len - n) xs


--between l u x = x > l && x < u


--mapIdx :: (Int -> b) -> [a] -> [b]
--mapIdx f xs = map f [0..length xs-1]

for2' :: [a] -> (a->[b]) -> (a -> b -> c) -> [[c]]
for2' xs f g = map (\x-> map (g x) $ f x) xs

sampleMany :: [Sampler a] -> Sampler [a]
sampleMany = sequence

sampleMany2 :: [[Sampler a]] -> Sampler [[a]]
sampleMany2 = sequence . map sequence

f >-> g = \x -> f x >>= g

metSampleP s = metSample1P s depSam
--metSamplePx0 x0  = metSample1P (depSamx0 x0)
--metSamplePCL s = metSample1PCL s depSam

depSam w x0 =  mutGaussAbs x0 $ w*0.005
depSamx0 x0 w _ =  mutGaussAbs x0 $ w*0.005

