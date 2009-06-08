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
import EvalM


{-chainM :: Monad m => (s -> [a] -> m s)  -> [a] -> s -> m (s, [a])
chainM f [] s = return (s, [])
chainM f l@(x:xs) s = do (s', l') <- f s x 
                         chainM f l' s'-}

data RunState = RS { rstDecls :: [Declare], 
                     rstSess :: Maybe Session,
                     rstDt :: Double,
                     rstTmax :: Double
                   }


help = putStrLn $ unlines [
        "runbugpan [toptions] file\n\nOptions: ",
        "\t-n\t\tStart new session",
        "\t-c\t\tContinue last session",
        "\t-t {seconds}\tSet run length",
        "\t-d {seconds}\tSet timestep\n"

 ]

main = do
  args <- getArgs
  --print args
  if null args 
    then help
    else dispatch (RS [] Nothing 0.001 1) args

dispatch rst ("-n":args) = do 
  s <- newSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args


dispatch rst ("-c":args) = do 
  s <- lastSession "/home/tomn/sessions/" 
  dispatch (rst {rstSess = Just s}) args

dispatch rst ("-d":dts:args) = dispatch (rst {rstDt = read dts}) args
dispatch rst ("-t":dts:args) = dispatch (rst {rstTmax = read dts}) args

dispatch rst (file:args) | head file /= '-' = do
  --print file
  ds <- fileDecls file
  dispatch (rst {rstDecls = rstDecls rst ++ ds}) args

dispatch rst [] = go rst

go (RS ds Nothing dt tmax) = do
  --mapM (putStrLn . ppDecl) ds
  let runTM = runTravM ds []
  let prg = snd . runTM $ transform
  ress <- execInStages prg dt tmax return
  mapM_ print ress
  mapM_ showSig ress
  

go (RS ds (Just sess) dt tmax) = do
  --get t0 from db
  tnow <- getClockTime
  let t0 = diffInS tnow $ tSessionStart sess
  runOnce dt t0 tmax ds sess
  print "done running"
  return ()

showSig (nm,(SigV t1 t2 dt sf)) = do
  mapM (putStrLn . ppVal ) $ map sf [0..9]
  return ()
showSig _ = return ()