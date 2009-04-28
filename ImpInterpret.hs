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
--import Array

data InterpState = IS { sigs :: [(String, V)],
                        evts :: [(String, [(Double,V)])] }

exec :: [Stmt] -> Double -> Double -> IO [(String, V)]
exec stmts dt tmax = 
    let prg =  filter inMainLoop stmts 
        fixEnvEs = ("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
        fixEnv = map (\(n,e)->(n,unEvalM $ eval (evalS fixEnv) e)) fixEnvEs
        evalS e =  extsEnv e $ EvalS 1 1 Nothing []
        initSigs = [ (nm, unEvalM $ eval (evalS fixEnv) e) | InitSig nm e <-  stmts]
        initEvts = [ (nm,ListV []) | EventAddRule nm _ <- stmts]
        bufnms = [ bnm | SigSnkConn _ ('#':bnm) <- prg ]
        outNms = [ nm | SigSnkConn nm "print" <- prg ]
        nsteps :: Int
        nsteps = round $ tmax/dt
        ts = map ((*dt) . realToFrac) [0..nsteps] in
    do envHT <- H.fromList H.hashString (initSigs++initEvts++fixEnv)
       forM_ outNms $ putStr . (++"\t")
       putStr "\n"
       forM_ ts $ \t-> do
         H.update envHT "seconds" (NumV . NReal $ t)
         forM_ prg $ \stm -> do 
                         sevals <- H.toList envHT                       
                         let es = evalS sevals 
                         case stm of 
                           SigUpdateRule nm (Switch ess er) -> do
                                    eslams <- forM ess (\(Var en, slam) -> (`pair` slam) 
                                                        `fmap` (unListV =<< fromJust `fmap` 
                                                                          H.lookup envHT en))
                                    let eslams' = map (onFst toHsTime . onFst head) . 
                                                  filter (not . null . fst) $ eslams
                                    if null eslams'
                                       then do H.update envHT nm $ unEvalM $ eval es er
                                               return ()
                                       else do
                                         let idx = maxIdx (map (fst. fst) eslams')
                                         let ((t,v), Lam vn se) =  eslams'!!idx
                                         H.update envHT nm $ unEvalM $ eval (extEnv (vn,v) es) se
                                         return ()
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
                                    putStr $ show v++"\t"
                                    return ()                                     
                           SigSnkConn sn bn@('#':bufnm) -> do 
                                    buf <- H.lookup envHT bn
                                    val <- fromJust `fmap` H.lookup envHT sn
                                    -- putStrLn $ "writing "++show val++"to buffer "++bufnm
                                    case buf of
                                      Just (ListV vs) -> H.update envHT bn $ ListV (val:vs)
                                      Nothing -> H.update envHT bn $ ListV [val]
                                    return ()
                           _ -> return ()
         when (not . null $ outNms) $ putStr "\n"
       forM_ (map fst initEvts) $ \enm-> do
         ListV es <- fromJust `fmap` H.lookup envHT enm
         putStrLn $ concat [enm , " -> ", show $ reverse es]
       forM_ bufnms $ \bufn-> do 
         --H.lookup envHT bufn >>= print
         Just (ListV buf) <- H.lookup envHT ('#':bufn)
         --let arr = array (0,nsteps-1) $ zip [0..nsteps-1] $ reverse buf
         let arr = reverse buf
         H.update envHT ('#':bufn) . SigV $ \t-> arr!!(round $ t/dt)
       H.toList envHT
         

pair a b = (a,b)

onFst :: (a->b) -> (a,c)->(b,c)
onFst f (x,y) = (f x, y)

onSnd :: (a->b) -> (c,a)->(c,b)
onSnd f (x,y) = (x, f y)

toHsTime :: V->(Double,V)
toHsTime (PairV (NumV nv) v) = (numToDouble nv,v)

appVs (ListV ws) (ListV vs) = ListV (ws++vs) 

maxIdx :: Ord a => [a] -> Int
maxIdx (x:xs) = mxIxAcc 1 0 x xs
    where mxIxAcc curI mI mV [] = mI
          mxIxAcc curI mI mV (x:xs) = if x>mV
                                        then mxIxAcc (curI+1) curI x xs
                                        else mxIxAcc (curI+1) mI mV xs

--lastEvent