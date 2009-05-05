module Database where

import EvalM
import Eval
import Expr
import Data.Maybe
import Data.List
import Numbers
import ImpInterpret
import Compiler
import Stages
import Traverse
import Transform
import Control.Monad


data Session = Session { events :: [(String, [(Double,V)])],
                         sigSegments :: [(String, [(Double, Double, Double->V)])],
                         programsRun :: [(Double, Double, [Declare])],
                         qenv :: [(String, V)],
                         sessPrelude :: [(String, V)]
                       }

emptySession = Session [] [] [] [] 

sessEvalState s = EvalS 0 0 Nothing (sessPrelude s)

addRunToSession :: [Declare] -> Double -> Double -> [(String, V)] -> Session -> Session
addRunToSession decls t0 tmax ress sess 
    = let nmsToStore = [ nm | SinkConnect (Var nm) "store" <- decls ]
          sigsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress of
                                Just (SigV sf) -> Just (nm, [(t0,tmax, \t->sf (t-t0))])
                                _ -> Nothing
          evtsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress of
                                Just (ListV evs) -> 
                                    Just (nm, map (\(PairV (NumV (NReal tm)) v)-> (tm+t0,v)) evs)
                                _ -> 
                                    Nothing
          newEvs = spliceAssocs evtsToStore (events sess)
          newSigSegs = spliceAssocs sigsToStore (sigSegments sess)
      in Session newEvs newSigSegs ((t0,t0+tmax, decls):programsRun sess) (qenv sess) (sessPrelude sess)

spliceAssocs :: Eq a => [(a,[b])] ->[(a,[b])]->[(a,[b])]
spliceAssocs = spliceAssocs' []
spliceAssocs' accum assoc1 [] =accum++assoc1
spliceAssocs' accum assoc1 ((key,vls):assoc2) 
    = case lookup key assoc1 of
        Just moreVls -> spliceAssocs' (filter ((/=key) . fst) assoc1) 
                        (assoc2) 
                        $ ((key,vls++moreVls)):accum 
        Nothing -> spliceAssocs' assoc1 (assoc2) $ ((key,vls)):accum 

runOnce :: Double -> Double -> Double -> [Declare] -> Session -> IO Session
runOnce dt t0 tmax ds sess = do
  let prel = map (\(n,v)->(n,Const v)) (sessPrelude sess)
  let runTM = runTravM ds prel
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude
  ress <- execInStages (complPrel++prg) dt tmax
  let nsess = addRunToSession ds t0 tmax ress sess
  return sess

data Q = QVar String 
       -- | Filter E Q
       -- | Map E Q
       | Apply E Q
       | Has Q Q
       | In Q Q
       | Around Q Q
       | Bind String Q Q

ask :: Session -> Q -> EvalM [V]
ask sess (QVar nm) 
    = case lookup nm $ qenv sess of
        Just (ListV vs) -> return vs
        Just v -> return [v]
        Nothing -> lookupInSigs
    where lookupInSigs =  case lookup nm $ sigSegments sess of
                            Just vls -> return $ map (\(_,_,sf)-> SigV sf) vls
                            Nothing -> lookupInEvts
          lookupInEvts = case lookup nm $ events sess of
                            Just vls -> return $ map (\(t,v)-> PairV (NumV . NReal $ t) (v)) vls
                            Nothing -> return []
ask sess (Apply lame q) = do vs <- ask sess q
                             mkList `fmap` eval (sessEvalState sess) (App lame (Const $ ListV vs))
ask sess (Bind nm q qrest) = do vs <- ask sess q
                                ask (sess {qenv = (nm,ListV vs): qenv sess}) qrest

mkList :: V -> [V]
mkList (ListV vs) = vs
mkList v = [v]