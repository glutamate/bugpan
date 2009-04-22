module ImpInterpret where

import Expr
import EvalM
import Eval
import Compiler
import Numbers
import Control.Monad.State.Strict
import Data.IORef

data InterpState = IS { sigs :: [(String, V)],
                        evts :: [(String, [(Double,V)])] }

exec :: [Stmt] -> Double -> Double -> IO ()
exec stmts dt tmax = 
    let prg =  filter isSigOrEvtS stmts 
        fixEnvEs = ("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
        emptyEvalS = EvalS 1 1 Nothing []
        fixEnv = map (\(n,e)->(n,unEvalM $ eval fixEvalS e)) fixEnvEs
        fixEvalS =  extsEnv fixEnv emptyEvalS
        initSigs = [(nm, unEvalM $ eval fixEvalS e) | InitSig nm e <-  stmts]
        initEvts = [ (nm,[]) | EventUpdateRule nm _ <- stmts]
        nsteps :: Int
        nsteps = round $ tmax/dt
        ts = map ((*dt) . realToFrac) [0..nsteps] in
    do sigs <- newIORef initSigs
       evts <- newIORef initEvts
       forM ts $ \t-> 
            do forM prg $ \stm -> do
                 case stm of 
                   SigUpdateRule nm e -> 

updateEvent :: EvalS -> E -> [V]
updateEvent es e = case unEvalM $ eval es e of 
                      ListV [] -> []
                      ListV vs -> vs
                      _ -> []


