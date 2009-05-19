module ImpInterpret where

import Expr
import EvalM
import Eval
import Compiler
import Traverse (ifM)
import Numbers
import Control.Monad.State.Strict
import Data.IORef
import qualified Data.HashTable as H
import Data.Maybe
import OpenGL
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import System.Time

--import Array

data InterpState = IS { sigs :: [(String, V)],
                        evts :: [(String, [(Double,V)])] }

exec :: [Stmt] -> Double -> Double -> IO [(String, V)]
exec stmts dt tmax = 
    let prg =  filter inMainLoop stmts 
        fixEnvEs = ("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
        fixEnv = map (\(n,e)->(n,unEvalM $ eval (evalS fixEnv) e)) fixEnvEs
        initSigs = [ (nm, unEvalM $ eval (evalS fixEnv) e) | InitSig nm e <-  stmts]
        initEvts = [ (nm,ListV []) | EventAddRule nm _ <- stmts]
        bufnms = [ bnm | SigSnkConn _ ('#':bnm) <- prg ]
        outNms = [ nm | SigSnkConn nm "print" <- prg ]
        nsteps :: Int
        nsteps = round $ tmax/dt
        ts = map ((*dt) . realToFrac) [0..nsteps] 
        screenVars = [ nm | SigSnkConn nm "screen" <- prg ]
        prgNoScreen = filter (noScreen screenVars) prg
        prgScreen = catMaybes . map unUpdateRule . filter (not . noScreen screenVars) $  prg
        runRealTime =  not . null $ prgScreen
    in
    do envHT <- H.fromList H.hashString (initSigs++initEvts++fixEnv)
       running <- newEmptyMVar
       lastPull <- newIORef 5
       largestPullLatency <- newIORef 0
       t0' <- getClockTime
       let screenPull = 
             --running <- readIORef started
            
                  do es <- evalS `fmap` H.toList envHT
                     tnow <- secsSince t0' -- (unsafeVToDbl . fromJust) `fmap` H.lookup envHT "seconds"
                     --putStrLn $ "pull at "++ show t
                     tlast <- readIORef lastPull
                     largest <- readIORef largestPullLatency
                     when ((tnow-tlast)>largest) $ writeIORef largestPullLatency (tnow-tlast)
                     writeIORef lastPull tnow
                     return . ListV $ map (unEvalM . eval es) prgScreen


       forM_ outNms $ putStr . (++"\t")
       putStr "\n"

       sequence_ $ map ($envHT) [ rp | RunPrepare rp <- prg ]

       --wait a bit and init screen
       when (not . null $ prgScreen ) (forkOS (runGlSignals screenPull running)  >> waitSecs 0.5)

       --get tnow
       t0 <- getClockTime
       sequence_ $ map ($envHT) [ tr | Trigger tr <- prg ]

       forkIO $ sequence_ $ map ($envHT) [ tr | RunAfterGo tr <- prg ]
       --print t0       
       forM_ ts $ \t-> do
         -- wait until t
         when (runRealTime) (waitUntil t0 t)

         H.update envHT "seconds" (NumV . NReal $ t)
         forM_ prgNoScreen $ \stm -> do 
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
                                       then do H.update envHT nm $! unEvalM $ eval es er
                                               return ()
                                       else do
                                         let idx = maxIdx (map (fst. fst) eslams')
                                         let ((t,v), Lam tn (Lam vn se)) =  eslams'!!idx
                                         H.update envHT nm $! unEvalM $ eval (extsEnv [(vn,v), (tn, NumV. NReal $ t)] es) se
                                         return ()
                           SigUpdateRule nm e -> do
                                    H.update envHT nm $! unEvalM $ eval es e
                                    return ()
                           EventAddRule  nm e -> do
                                    evs<-fromJust `fmap` H.lookup envHT nm
                                    let newevs = unEvalM $ eval es e
                                    H.update envHT nm $! (appVs newevs evs) 
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
                           ReadSrcAction nm src -> do vl <- src t dt
                                                      H.update envHT nm vl
                                                      return ()
                           _ -> return ()
         tryPutMVar running ()
         when (not . null $ outNms) $ putStr "\n"
       --done
       takeMVar running 

       sequence_ $ map ($envHT) [ rad | RunAfterDone rad <- prg ]

       forM_ (map fst initEvts) $ \enm-> do
         ListV es <- fromJust `fmap` H.lookup envHT enm
         putStrLn $ concat [enm , " -> ", show $ reverse es]
       forM_ bufnms $ \bufn-> do 
         --H.lookup envHT bufn >>= print 
         Just (ListV buf) <- H.lookup envHT ('#':bufn)
         --let arr = array (0,nsteps) $ zip [0..nsteps] $ reverse buf
         let arr = reverse buf
         H.update envHT ('#':bufn) . SigV 0 tmax dt $ \t-> arr!!(round $ t/dt)
       readIORef largestPullLatency >>= putStrLn . ("largest latency: "++) . show
       H.toList envHT 

         
{-globalSecsNow :: IO Double
globalSecsNow = do tnow <- getClockTime
                   tstart <- readTV globalTimeStartTVar
                   return $ diffInS tnow tstart   -}               

diffInS (TOD t1s t1ps) (TOD t2s t2ps) = (fromInteger $ (t1s-t2s)*1000*1000 + ((t1ps-t2ps) `div` (1000*1000))) / 1000000

waitSecs :: Double -> IO ()
waitSecs s = threadDelay . round $ s*1000*1000


waitUntil t0 s = do tn <- getClockTime
                    let elapsed = diffInS tn t0
                    --print elapsed
                    if elapsed < s
                       then threadDelay . round $ (s-elapsed)*1000*1000
                       else return () 
 
secsSince t0 =  do tn <- getClockTime
                   return $  diffInS tn t0

noScreen _ (SigSnkConn _ "screen") = False
noScreen nms (SigUpdateRule nm _) | nm `elem` nms = False
                                  | otherwise = True
noScreen _ _ = True

unUpdateRule (SigUpdateRule _ e) = Just e
unUpdateRule _ = Nothing

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