module Database where

import EvalM
import Expr
import Data.Maybe
import Data.List
import Numbers
import ImpInterpret
import Compiler
import Stages
import Traverse
import Transform


data Session = Session { events :: [(String, [(Double,V)])],
                         sigSegments :: [(String, [(Double, Double, Double->V)])],
                         programsRun :: [(Double, [Declare])]
                       }

emptySession = Session [] [] []

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
      in Session newEvs newSigSegs $ (t0,decls):programsRun sess

spliceAssocs :: Eq a => [(a,[b])] ->[(a,[b])]->[(a,[b])]
spliceAssocs = spliceAssocs' []
spliceAssocs' accum assoc1 [] =accum++assoc1
spliceAssocs' accum assoc1 ((key,vls):assoc2) 
    = case lookup key assoc1 of
        Just moreVls -> spliceAssocs' (filter ((/=key) . fst) assoc1) 
                        (assoc2) 
                        $ ((key,vls++moreVls)):accum 
        Nothing -> spliceAssocs' assoc1 (assoc2) $ ((key,vls)):accum 

run :: Double -> Double -> [Declare] -> [Declare] -> Session -> IO Session
run dt tmax prel ds sess = do
  let runTM = runTravM ds (declsToEnv prel)
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude
  ress <- execInStages (complPrel++prg) dt tmax
  let nsess = addRunToSession ds 0 tmax ress sess
  return sess
