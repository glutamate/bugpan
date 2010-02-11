module FitGnuplot where

import PlotGnuplot
import NewSignal
import QueryPlots
import TNUtils
import Data.List
import QueryTypes
import Numeric.GSL.Minimization
import qualified Data.StorableVector as SV


data Fit = Fit String [(String, Double)] (Signal Double)
         
data FitG = FitG ([Double] -> Double -> Double) [Double] (Signal Double)

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

fitG :: ([Double] -> Double -> Double) -> [Double] -> [Signal Double] -> [Duration [Double]]
fitG f inits sigs = map g sigs
    where g sig@(Signal t1 t2 _ _ _) = ((t1,t2), fitG' f inits sig)

fitG' :: ([Double] -> Double -> Double) -> [Double] -> (Signal Double) -> [Double]
fitG' f inits sig@(Signal t1 t2 dt arr Eq) = 
   let n = SV.length arr 
       square x = x*x
       g arg = let predarr = SV.sample n (\i->f arg ((t1+) $ realToFrac i*dt))
               in SV.foldl1' (+) $ SV.zipWith (\x y -> square(x-y)) predarr arr
   in fst $ minimize NMSimplex2 1E-4 500 (map (/10) inits) g inits
fitG' f inits sig = fitG' f inits $ forceSigEq sig

fitS :: ([Double] -> Double -> Double) -> [Double] -> [Signal Double] -> [Signal Double]
fitS f inits sigs = map (fitS' f inits . forceSigEq) sigs
    where fitS' ::([Double] -> Double -> Double) -> [Double] -> Signal Double -> Signal Double
          fitS' f inits sig@(Signal t1 t2 dt arr Eq) = 
              let n = SV.length arr 
                  square x = x*x
                  predarr arg = SV.sample n (\i->f arg (realToFrac i*dt))
                  g arg = SV.foldl1' (+) $ SV.zipWith (\x y -> square(x-y)) (predarr arg) arr
                  soln = fst $ minimize NMSimplex2 1E-5 500 (map (/10) inits) g inits
              in Signal t1 t2 dt (predarr soln) Eq


instance PlotWithGnuplot FitG where
    getGnuplotCmd (FitG f inits sig@(Signal t1 t2 dt _ _)) = 
        let soln = fitG' f inits sig
        in getGnuplotCmd $ FunSeg t1 t2 $ f soln
       
                     
instance QueryResult FitG where
    qFilterSuccess _ = True
    qReply (FitG eqn initVals sig) _ = do
                            let sol = fitG' eqn initVals sig
                            return $ show sol
 

mainTest = fit "a*x*x+b*x+c" [("a", 1), ("b", 2), ("c", 3)] sineSig
mainTest1 = fitG' (\[a,b,c] x-> a*x*x+b*x+c) [1,2,3] sineSig