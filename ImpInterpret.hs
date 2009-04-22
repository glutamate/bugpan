module ImpInterpret where

import Expr
import EvalM
import Eval
import Compiler
import Numbers

exec :: [Stmt] -> Double -> Double -> IO ()
exec stmts dt tmax = 
    let initsigs = [(nm,v) | InitSig nm v <-  stmts]
        prg =  filter isSigOrEvtS stmts 
        fixEnvEs = ("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
        emptyEvalS = EvalS 1 1 Nothing []
        fixEnv = map (\(n,e)->(n,unEvalM $ eval fixEvalS e)) fixEnvEs
        fixEvalS =  extsEnv fixEnv emptyEvalS
        nsteps :: Int
        nsteps = round $ tmax/dt
        ts = map ((*dt) . realToFrac) [0..nsteps]
    in undefined

updateEvent :: EvalS -> E -> [V]
updateEvent es e = case unEvalM $ eval es e of 
                      ListV [] -> []
                      ListV vs -> vs
                      _ -> []


