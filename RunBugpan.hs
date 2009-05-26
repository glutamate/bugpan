module Main where

import Parse
import Expr
import Database
import System.Environment
import Database
import System.Time
import ImpInterpret
import Traverse
import Transform
import Stages


{-chainM :: Monad m => (s -> [a] -> m s)  -> [a] -> s -> m (s, [a])
chainM f [] s = return (s, [])
chainM f l@(x:xs) s = do (s', l') <- f s x 
                         chainM f l' s'-}

data RunState = RS { rstDecls :: [Declare], 
                     rstSess :: Maybe Session,
                     rstDt :: Double,
                     rstTmax :: Double
                   }

main = do
  args <- getArgs
  dispatch (RS [] Nothing 0.001 1) args

dispatch rst ("-n":args) = do 
  s <- newSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args


dispatch rst ("-c":args) = do 
  s <- lastSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args

dispatch rst ("-d":dts:args) = dispatch (rst {rstDt = read dts}) args
dispatch rst ("-t":dts:args) = dispatch (rst {rstTmax = read dts}) args

dispatch rst (file:args) | head file /= '-' = do
  ds <- fileDecls file
  dispatch (rst {rstDecls = rstDecls rst ++ ds}) args

dispatch rst [] = go rst

go (RS ds Nothing dt tmax) = do
  let runTM = runTravM ds []
  let prg = snd . runTM $ transform
  ress <- execInStages prg dt tmax return
  mapM_ print ress

go (RS ds (Just sess) dt tmax) = do
  --get t0 from db
  tnow <- getClockTime
  let t0 = diffInS tnow $ tSessionStart sess
  runOnce dt t0 tmax ds sess
  return ()
