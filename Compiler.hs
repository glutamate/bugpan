module Compiler where

import Expr
import EvalM
import System.Random
import Data.HashTable as H
import Daq
import Numbers
import Control.Concurrent
import Statement
import BuiltIn
import PrettyPrint
import RandomSources
import OpenGL (setTexture)

compile :: [Declare] -> [Stmt]
compile ds = let c = concatMap compileDec (filter noDtSeconds (bivDecls++ds)) in 
             concat [constEnv c, initSigVals c, mainLoop c] 

noDtSeconds :: Declare -> Bool
noDtSeconds (Let (PatVar "dt" _) _) = False
noDtSeconds (Let (PatVar "seconds" _) _) = False
noDtSeconds _ = True
                 

compileDec :: Declare -> [Stmt]
compileDec (Let (PatVar nm _) (Sig se)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let (PatVar nm _) (SigLimited se lim)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let (PatVar nm _) (SigDelay (Var sn) v0)) = [SigUpdateRule nm (Var sn), 
                                              InitSig nm v0]
compileDec (Let (PatVar nm _) (Event ee)) = [EventAddRule nm $ unVal ee]
compileDec (Let (PatVar nm _) (Forget tm ee)) = [EventAddRule nm $ Forget tm $ unVal ee]
compileDec (Let (PatVar nm _) (Switch ses ser)) = 
               [SigUpdateRule nm (Switch (map noSig ses) $ unSig (unVal ser))]
    where noSig (e,s) = (e, unVal $ mapE unSig s)
          unSig (Sig se) = se 
          unSig e = e
compileDec rs@(ReadSource nm ("adc", _)) = compileAdcSrc rs
{-compileDec (ReadSource nm ("loadTexture", (Const fnm))) = 
   [RunInGLThread $ \env -> do 
          setTexture $ (unsafeReify fnm)::String
          print "HELLLOOOO!!!!!!!!!!!!"
          update env nm $ BoxV (p3 1 1 1) (p3 (-0.5) (-0.5) 0) (p3 1 1 1) 
          return () ] -}
compileDec (ReadSource nm (srcNm, (Const arg))) = [ReadSrcAction nm $ genSrc srcNm arg]
compileDec (Let (PatVar nm _) e) = [Env nm $ unVal e]
compileDec (SinkConnect (Var nm) (snkNm,_)) = [SigSnkConn nm snkNm]
compileDec (Stage _ _) = []
compileDec (DeclareType _ _) = []
compileDec s = error $ "compileDec: unknown decl "++show s

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

inMainLoop (SigUpdateRule _ _) = True
inMainLoop (EventAddRule _ _) = True
inMainLoop (SigSnkConn _ _) = True 
inMainLoop (ReadSrcAction _ _) = True
inMainLoop (RunPrepare _) = True
inMainLoop (RunInGLThread _) = True
inMainLoop (RunAfterDone _) = True
inMainLoop (RunAfterGo _) = True
inMainLoop (Trigger _) = True
inMainLoop (GLParams _ _) = True
--inMainLoop (ReadSrcAction _ _) = True
inMainLoop _ = False

ppStmt :: Stmt -> String
ppStmt (InitSig n e) = concat [n, "(0) = ", pp e]
ppStmt (Env n e) =  concat [n, " = ", pp e]
ppStmt (SigUpdateRule n e) =  concat [n, " = {: ", pp e, " :}"]
ppStmt (EventAddRule n e) =  concat [n, " = [: ", pp e, " :]"]
ppStmt (SigSnkConn vn sn) = concat [vn, " *> ", sn]
ppStmt (ReadSrcAction nm _) = nm ++ " <- <signal source>"
ppStmt (RunPrepare _) = "prepare something"
ppStmt (RunInGLThread _) = "run something in GL throead"
ppStmt (RunAfterDone _) = "run something after done"
ppStmt (RunAfterGo _) = "run something after go"
ppStmt (Trigger _) = "trigger somehow"
ppStmt (GLParams _ _) = "gl parameters"

initSigVals stmts = [is | is@(InitSig nm v) <-  stmts]
constEnv stmts = [en | en@(Env nm v) <-  stmts]
mainLoop  stmts =  filter inMainLoop stmts

genSrc :: String -> V -> (RealNum -> RealNum -> IO V)
genSrc "bernoulli" rateS t dt = 
    do rnd <- randomRIO (0,1)
       return . BoolV $ rnd < ( unsafeReify rateS)*dt
genSrc "uniform" (PairV lo hi) t dt = 
    do rnd <- randomRIO ( unsafeReify lo, unsafeReify hi)
       return . NumV . NReal  $ rnd
genSrc nms _ _ _ = error $ "unknown source: "++show nms

p3 x y z = (PairV (PairV x y) z)

{- note: Now, 

Event :: [(T,a)] -> Event a

what if

Event :: ([(T,a)] -> [(T,a)]) -> Event a

or something like that
-}
