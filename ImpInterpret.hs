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
import OpenGL (initGlScreen)
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import System.Time
import Statement
import TNUtils
import Data.Array
import PrettyPrint
import HaskSyntaxUntyped

exec :: [Stmt] -> RealNum -> RealNum -> IO [(String, V)]
exec stmts dt tmax = 
    let prg =  filter inMainLoop stmts 
        fixEnvEs = ("tmax",Const . NumV . NReal$ tmax):("dt",Const . NumV . NReal$ dt):[(nm,v) | en@(Env nm v) <-  stmts]
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
        runningMV = safeHead [ runmv | GLParams _ runmv <- prg]
        dispPullMV = safeHead [ dpmv | GLParams dpmv _ <- prg]
    in
    do envHT <- H.fromList H.hashString (initSigs++initEvts++fixEnv)
       print "############hello form impinterpret"
--       mapM (putStrLn . ppStmt)  prg
       mapM print prg
       print "############hello form impinterpret"
       lastPull <- newIORef 5
       largestPullLatency <- newIORef 0
       t0' <- getClockTime
       print2 "buffer: " bufnms
       
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

       (runMV, dispMV) <- cond [
                             (null prgScreen, return (Nothing,Nothing))
                           , (isJust dispPullMV && isJust runningMV, return (runningMV, dispPullMV))]
                           $ do
                                rMV <- newEmptyMVar
                                scrPl <- newEmptyMVar
                                forkOS $ initGlScreen False scrPl rMV 
                                               (sequence_  [ rp envHT | RunInGLThread rp <- prg ])
                                waitSecs 0.5
                                return (Just rMV, Just scrPl)
                             
       sequence_ $ map ($envHT) [ rp | RunPrepare rp <- prg ]
                          
       maybeM dispMV $  \dpmv -> putMVar dpmv screenPull

--if ((not . null $ prgScreen) && isNothing (dispPullMV) 
  --         then
       --wait a bit and init screen
       -- when ((not . null $ prgScreen) && isNothing (dispPullMV) ) (

       --get tnow
       t0 <- getClockTime
       putStrLn $ "trigger at "++show t0++" dt="++show dt++" tmax="++show tmax

       sequence_ $ map ($envHT) [ tr | Trigger tr <- prg ]

       sequence_ $ map ($envHT) [ tr | RunAfterGo tr <- prg ]
       --print t0       
       forM_ ts $ \t-> do
         -- wait until t
         when (runRealTime) (waitUntil t0 t)

         H.update envHT "seconds" (NumV . NReal $ t)
         let seconds = (NumV . NReal $ t)
         forM_ prgNoScreen $ \stm -> do 
                         --putStrLn $ ppStmt stm
                         sevals <- H.toList envHT                       
                         let es = evalS sevals 
                             evalToEnv env nm e =  case eval (extsEnv env es) e of
                                              Res v ->  (H.update envHT nm $! v) >> return ()
                                              Error s -> fail $ "eval error in exec "++nm++" => "++pp e++": "++s
                             evalTo = evalToEnv []  
                         case stm of 
                           SigUpdateRule nm sw@(Switch ess er) -> do
                                    --putStrLn $ nm++": "++ pp sw
                                    eslams <- forM ess (\(Var en, slam) -> (`pair` slam) 
                                                        `fmap` (unListV =<< fromJust `fmap` 
                                                                          H.lookup envHT en))
                                   -- print 1
                                    let eslams' = map (onFst toHsTime . onFst head) . 
                                                  filter (not . null . fst) $ eslams
                                    --print 2
                                    if null eslams'
                                       then evalTo nm er
                                       else do
                                         --print 4
                                         let idx = maxIdx (map (fst. fst) eslams')
                                         let ((t,v), Lam tn tt (Lam vn tv se)) =  eslams'!!idx
                                         evalToEnv [(vn,v), (tn, NumV. NReal $ t)] nm se
                           SigUpdateRule nm e -> do
                                    --putStrLn $ nm++": "++ pp e
                                    evalTo nm e
                                    return ()
                           EventAddRule  nm (Forget (Const (NumV (NReal tm))) e) -> do
                                    --print $ "updating "++nm
                                    evs<-fromJust `fmap` H.lookup envHT nm
                                    let newevs = case eval es e of
                                                   Res v -> v
                                                   Error s-> error $ "error in updating "++nm++": s"
                                    H.update envHT nm $! ListV $ filter ((>(t-tm)) .unsafeReify . fstV) $  (appVs newevs evs) 
                                    --print $ "done"
                                    return ()
                           EventAddRule  nm e -> do
                                    --print $ "updating "++nm
                                    evs<-fromJust `fmap` H.lookup envHT nm
                                    let newevs = case eval es e of
                                                   Res v -> v
                                                   Error s-> error $ "error in updating "++nm++": s"
                                    H.update envHT nm $! ListV (appVs newevs evs) 
                                    --print $ "done"
                                    return ()
                           SigSnkConn nm "print" -> do 
                                    v <- H.lookup envHT nm
				    case v of 
					Just v' -> putStr $ ppVal v'++"\t"
					Nothing -> putStr $ "noval"++"\t"
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
         maybeM runMV (\mv-> tryPutMVar mv ())
         when (not . null $ outNms) $ putStr "\n"
       --done

       maybeM runMV takeMVar

       maybeM dispMV $  \dpmv -> takeMVar dpmv 

       sequence_ $ map ($envHT) [ rad | RunAfterDone rad <- prg ]

       --ideally, reverse (some!) events

       forM_ (map fst initEvts) $ \enm-> do
         ListV es <- fromJust `fmap` H.lookup envHT enm
         putStrLn $ concat [enm , " -> ", concatMap ppVal $ reverse es]
       forM_ bufnms $ \bufn-> do 
         --H.lookup envHT bufn >>= print 
         lubuf <- H.lookup envHT ('#':bufn)
         case lubuf of
            Just (ListV buf) -> do
                                  let arr = array (0,nsteps) $ zip [0..nsteps] $ reverse buf
                                  --let arr = reverse buf
                                  H.update envHT ('#':bufn) . SigV 0 tmax dt $ \t-> arr!t
                                  return ()
            _ -> return ()
         lusbuf <- H.lookup envHT ('%':bufn)
         case lusbuf of
           Just v -> do H.delete envHT ('%':bufn)
                        H.update envHT ('#':bufn) v
                        return ()
           _ -> return ()
              
       readIORef largestPullLatency >>= putStrLn . ("largest latency: "++) . show
       H.toList envHT 


        


noScreen _ (SigSnkConn _ "screen") = False
noScreen nms (SigUpdateRule nm _) | nm `elem` nms = False
                                  | otherwise = True
noScreen _ _ = True

unUpdateRule (SigUpdateRule _ e) = Just e
unUpdateRule _ = Nothing


toHsTime :: V->(RealNum,V)
toHsTime (PairV (NumV nv) v) = (numToDouble nv,v)

appVs (ListV ws) (ListV vs) = (ws++vs) 


--lastEvent
