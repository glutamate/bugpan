module ImpInterpret where

import Expr
import EvalM
import Eval
import Compiler
import Numbers
import Control.Monad.State.Strict
import Data.IORef
import qualified Data.HashTable as H
import Data.Maybe

data InterpState = IS { sigs :: [(String, V)],
                        evts :: [(String, [(Double,V)])] }

exec :: [Stmt] -> Double -> Double -> IO ()
exec stmts dt tmax = 
    let prg =  filter inMainLoop stmts 
        fixEnvEs = ("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
        fixEnv = map (\(n,e)->(n,unEvalM $ eval (evalS fixEnv) e)) fixEnvEs
        evalS e =  extsEnv e $ EvalS 1 1 Nothing []
        initSigs = [ (nm, unEvalM $ eval (evalS fixEnv) e) | InitSig nm e <-  stmts]
        initEvts = [ (nm,ListV []) | EventAddRule nm _ <- stmts]
        outNms = [ nm | SigSnkConn nm "print" <- prg ]
        nsteps :: Int
        nsteps = round $ tmax/dt
        ts = map ((*dt) . realToFrac) [0..nsteps] in
    do envHT <- H.fromList H.hashString (initSigs++initEvts++fixEnv)
       forM_ outNms $ putStr . (++"\t")
       putStr "\n"
       forM_ ts $ \t-> do
         forM_ prg $ \stm -> do 
                         sevals <- H.toList envHT
                         let es = evalS $ ("seconds", NumV . NReal $ t):sevals 
                         case stm of 
                          -- SigUpdateRule nm (Switch ess er) -> do
                                    
                           SigUpdateRule nm e -> do
                                    H.update envHT nm $ unEvalM $ eval es e
                                    return ()
                           EventAddRule  nm e -> do
                                    evs<-fromJust `fmap` H.lookup envHT nm
                                    let newevs = unEvalM $ eval es e
                                    H.update envHT nm (appVs newevs evs) 
                                    return ()
                           SigSnkConn nm "print" -> do 
                                    v <-fromJust `fmap` H.lookup envHT nm
                                    print v
                                    return ()                                     
                           _ -> return ()
       forM_ (map fst initEvts) $ \enm-> do
         es <- fromJust `fmap` H.lookup envHT enm
         putStrLn $ concat [enm , " -> ", show es]
         

appVs (ListV ws) (ListV vs) = ListV (ws++vs) 