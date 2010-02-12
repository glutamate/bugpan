module FitGnuplot where

import PlotGnuplot
import NewSignal
import QueryPlots
import TNUtils
import Data.List
import Data.Ord
import QueryTypes
import Numeric.GSL.Minimization
import Math.Probably.GlobalRandoms
import Math.Probably.Sampler
import qualified Data.StorableVector as SV
import qualified Control.Exception as C
import Data.Maybe
import System.IO.Unsafe

data Fit = Fit String [(String, Double)] (Signal Double)
         
data FitG = FitG ([Double] -> Double -> Double) (Sampler [Double]) (Signal Double)

instance QueryResult Fit where
    qFilterSuccess _ = True
    qReply (Fit eqn initVals sig) _ = do
                           nm <- fit eqn initVals sig
                           return $ "\\input{"++nm++"}"
                                    

fit :: String -> [(String, Double)] -> Signal Double -> IO String
fit eqn initVals sig@(Signal t1 t2 dt _ _) = do
  u <- uniqueIntStr
  let fnm = "/tmp/fitgnuplotsig"++ u
  writeSig fnm $ forceSigEq sig
  let format = "format=\"%float64\" using ($0*"++^dt++"+"++^ t1++"):1"
  --putStrLn format             
  let cmd=unlines $ ["set terminal latex",
                     "set output 'fitplot"++u++"'",
                     "pos(x)=x<0?0:x",
                     "f(x) = "++eqn]++
                     map (\(nm,vl)-> nm++"="++show vl) initVals++
                     ["fit f(x) '"++fnm++"' binary "++format++" via "++
                      (intercalate ", " $ map fst initVals),
                      "plot '"++fnm++"' binary "++format++" title 'data', "++
                      eqn++" title 'fit'"
                     ]
  --putStrLn cmd
  execGP cmd

  return $ "fitplot"++u

    where x++^y = x++show y

fitG :: ([Double] -> Double -> Double) -> Sampler [Double] -> [Signal Double] -> [Duration [Double]]
fitG f inits sigs = map g sigs
    where g sig@(Signal t1 t2 _ _ _) = ((t1,t2), fst3 $ fitG' f inits sig)

fitG' :: ([Double] -> Double -> Double) -> Sampler [Double] -> Signal Double -> ([Double], Signal Double, Double)
fitG' f initsam sig@(Signal t1 t2 dt arr Eq) = 
    let fitsam = do
          inits <- initsam
          return $ fitG1 f inits sig
        manyRes = catMaybes $ sampleN 10 fitsam
    in minimumBy (comparing trd3) manyRes

fitG1 :: ([Double] -> Double -> Double) -> [Double] -> Signal Double -> Maybe ([Double], Signal Double, Double)
fitG1 f inits sig@(Signal t1 t2 dt arr Eq) = 
   let n = SV.length arr 
       square x = x*x
       predarr arg = SV.sample n (\i->f arg (realToFrac i*dt))
       g arg = SV.foldl1' (+) $ SV.zipWith (\x y -> square(x-y)) (predarr arg) arr
       soln = unsafePerformIO $ C.catch (return $ Just $ minimize NMSimplex2 1E-5 500 (map (/10) inits) g inits)
                                        (\e-> const (return Nothing) (e::C.SomeException))
       --soln = Just $ minimize NMSimplex2 1E-5 500 (map (/10) inits) g inits
   in case soln of
        Just s -> Just (fst s, Signal t1 t2 dt (predarr $ fst s) Eq, g $ fst s)
        Nothing -> Nothing
fitG1 f inits sig = fitG1 f inits $ forceSigEq sig

fitS :: ([Double] -> Double -> Double) -> Sampler [Double] -> [Signal Double] -> [Signal Double]
fitS f initsam sigs = map (snd3 . fitG' f initsam . forceSigEq) sigs

instance PlotWithGnuplot FitG where
    getGnuplotCmd (FitG f inits sig@(Signal t1 t2 dt _ _)) = 
        let soln = fst3 $ fitG' f inits sig
        in getGnuplotCmd $ FunSeg t1 t2 $ f soln
       
                     
instance QueryResult FitG where
    qFilterSuccess _ = True
    qReply (FitG eqn initVals sig) _ = do
                            let (sol, _, err) = fitG' eqn initVals $ forceSigEq sig
                            return $ show (err,sol)
 

mainTest = fit "a*x*x+b*x+c" [("a", 1), ("b", 2), ("c", 3)] sineSig
mainTest1 = fitG' (\[a,b,c] x-> a*x*x+b*x+c) (return [1,2,3]) sineSig