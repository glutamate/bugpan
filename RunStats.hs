module Main where

import Parse
import Expr
import System.Environment
import Database
import Traverse
import EvalM
import Numbers 
import Control.Monad
import TNUtils
import Query (bugpanRootDir)

help = putStrLn $ unlines [
        "runstats {model file}\n\n"
        ]       

main = do
  args <- getArgs
  --print args 
  if null args 
    then help
    else dispatch args

dispatch (filenm:_) = do
  ds <- fileDecls filenm []
  mapM print ds

  return ()

rvars :: [Declare] -> [(String, Int)]
rvars = concatMap rvar 
   where rvar (Distribute p e) = [(unsafePatToName p, 0)]
         rvar _ = []