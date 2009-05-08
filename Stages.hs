module Stages where

import Expr
import Traverse
import Data.List (partition, nub, (\\))
import Transform
import ImpInterpret
import Compiler
import Data.IORef
import Control.Monad
import EvalM

--data RelPos = At | Before | After

--type Target = Either String Int

-- 1. look for stage annotatinos
-- 2. for each anno, sigs it depends on get same stage anno

splitByStages :: [Declare] -> [[Declare]]
splitByStages ds = 
    let stages = nub [ s | Stage _ s <- ds ]
        (mainL, env) = partition declInMainLoop ds 
        stageDs st = let nms = [ nm | Stage nm s <- ds, s==st ]
                         in [ d | d@(Let nm _) <- ds, nm `elem` nms ]++
                            [ d | d@(SinkConnect _ ('#':nm)) <- ds, nm `elem` nms]
        stagedDecls = map stageDs stages
        unstagedDecls = mainL \\ (concat stagedDecls)
    in (env : stagedDecls) ++ [unstagedDecls]

execInStages :: [Declare] -> Double -> Double -> IO [(String,V)]
execInStages ds dt tmax = do
  let (env:stageDs) = splitByStages ds
  envAdd <- newIORef []
  --putStrLn "\nenvironment"
  --mapM (putStrLn . ppDecl ) env
  forM_ stageDs $ \decls -> do
                            envAdded <- readIORef envAdd -- also change sigat nm to sigat #nm
                            let copyEnvSigs = [ Let nm (Sig $ SigAt (Var "seconds") (Var ('#':nm))) | ('#':nm,_) <- envAdded ]
                            let stmts = compile $ env++copyEnvSigs++map envToDecl envAdded++decls
                            --putStrLn "\na stage"
                            --mapM (putStrLn . ppStmt ) stmts
                            let buffered = [ nm | SinkConnect (Var _) ('#':nm)<- decls ]
                            ress <- exec stmts dt tmax
                            let savedRess = [('#':nm, val) | ('#':nm, val) <- ress, nm `elem` buffered ]
                            addToIORefList envAdd savedRess
                            let storeResNms = [nm | SinkConnect (Var nm) "store" <- decls ]  
                            let storeResVls = [(nm, val) | (nm, val) <- ress, 
                                                                        nm `elem` storeResNms, 
                                                                        not $ nm `elem` buffered]
                            addToIORefList envAdd storeResVls

  readIORef envAdd
                            


envToDecl (nm, val) = Let nm $ Const val

addToIORefList :: IORef [a] -> [a] -> IO ()
addToIORefList ioref xs = do ys <- readIORef ioref
                             writeIORef ioref (xs++ys)
                            
       