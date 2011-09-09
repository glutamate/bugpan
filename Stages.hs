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
import Statement
import Database
import TNUtils
import Numbers
import PrettyPrint

--data RelPos = At | Before | After

--type Target = Either String Int

-- 1. look for stage annotatinos
-- 2. for each anno, sigs it depends on get same stage anno


execInStages :: [Declare] -> RealNum -> RealNum -> ([Stmt] -> IO [Stmt]) -> IO [(String,V)]
execInStages ds dt tmaxGlobal postCompile = do
  let (env:stageDs) = splitByStages ds
  envAdd <- newIORef []
  --putStrLn "\nenvironment"
  --mapM (putStrLn . ppDecl ) env
  forM_ stageDs $ \decls' -> do
                            envAdded <- readIORef envAdd -- also change sigat nm to sigat #nm
                            let sigsAdded = [ nm | ('#':nm,sig) <- envAdded ]
                            print2 "sigsAdded" sigsAdded
                            --let copyEnvSigs = [ Let nm (Sig $ SigAt (Var "seconds") (Var ('#':nm))) | 
                            --                                  ('#':nm,_) <- envAdded ]
                            --instead, subst #sig for sig
                            let decls = snd $ runTravM decls' [] (mapDE (mapEM $ substSigRefs sigsAdded))
                            let stmts' = compile $ env++map envToDecl envAdded++decls
                            putStrLn "\na stage"
                            mapM (putStrLn . ppDecl ) decls
                            let buffered = [ nm | SinkConnect (Var _) ('#':nm,_ )<- decls ]
                            let tmax = localTmax tmaxGlobal decls
                            stmts <- postCompile stmts'
                            ress <- exec stmts dt tmax
                            let savedRess = [('#':nm, val) | ('#':nm, val) <- ress, nm `elem` buffered ]
                            addToIORefList envAdd savedRess
                            let storeResNms = [nm | SinkConnect (Var nm) ("store",arg) <- decls ]  
                            let storeResVls = [(nm, val) | (nm, val) <- ress, 
                                                                        nm `elem` storeResNms, 
                                                                        not $ nm `elem` buffered]
                            print storeResVls

                            addToIORefList envAdd storeResVls

  readIORef envAdd
                            

substSigRefs nmsigs (Var nm) | nm `elem`  nmsigs = return $ Var $ '#':nm
                             | otherwise = return . Var $ nm
substSigRefs _ e = return e 


envToDecl (nm, val) = Let (PatVar nm UnspecifiedT) $ Const val

addToIORefList :: IORef [a] -> [a] -> IO ()
addToIORefList ioref xs = do ys <- readIORef ioref
                             writeIORef ioref (xs++ys)
                            
       

runOnce :: RealNum -> RealNum -> RealNum -> [Declare] -> Session -> IO ()
runOnce dt t0 tmax ds sess = do
  --let prel = map (\(n,v)->(n,Const v)) (sessPrelude sess)
  --let runTM = runTravM ds []
  --mapM (putStrLn . ppDecl) ds
  let prg = snd . runTravM ds [] $ (transform >> evalSinkSrcArgs)
  --mapM (putStrLn . ppDecl)  prg
  ress <- execInStages prg dt tmax return
  --putStrLn $ "results for this trial: "++show ress
  addRunToSession ds t0 tmax dt ress sess
  print "runOnce done"
  return ()


runNtimes :: Int -> RealNum -> RealNum -> RealNum -> RealNum -> [Declare] -> Session -> IO ()
runNtimes 0 _   _    _      _    _  _  = return ()
runNtimes n dt tmax tstart tsep ds sess = do
  --print "first"
  runOnce dt tstart tmax ds sess 
  runNtimes (n-1) dt tmax (tstart+tsep+tmax) tsep ds sess

