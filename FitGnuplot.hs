module FitGnuplot where

import PlotGnuplot
import NewSignal
import QueryPlots
import TNUtils
import Data.List
import QueryTypes

data Fit = Fit String [(String, Double)] (Signal Double)

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


mainTest = fit "a*x*x+b*x+c" [("a", 1), ("b", 2), ("c", 3)] sineSig