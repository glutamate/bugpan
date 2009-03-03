{-# LANGUAGE NoMonomorphismRestriction, PatternSignatures #-} 
module Run where

import Expr
import Eval
import EvalM
 
import SrcSinks
import Control.Monad
import Data.IORef
import Numbers
import Data.Maybe
import Data.List
import Data.Unique
import System.IO.Unsafe

--import Data.Either

--to do:
-- +sep test from runner
-- +events
-- +delay
-- +no more pull sig
-- +event does not have oto be in outer
-- +plot sink

-- +let rec
-- +let rec for signals?
{- euler:
 dy/dt = f(y, t) and y(0) = y0

then

y(t+dt) = y(t) + dt * f(y(t), t)

integrate :: (a->Signal a) -> a -> Signal a
integrate f y0 = s 
    where yt = SigVal $ SigDelay s
           s = Sig $ yt+ dt * SigVal (f (yt)) 


start simpler:

iterate :: (a->a) -> a -> Signal a
intsig :: Signal a -> Signal a

why do i need sigdelay ?

-integrate
-differentiate
-filter? 

-}

-- +compiled prelude
-- case, fst and snd
-- sigdelay takes 2 args (for initital val)

-- integrate other sig

-- poisson events
-- convolution

-- solve ode
-- try int fire 
 
-- mouse input, gfx output

-- database sinks

-- daq sinks+srcs 
-- parser

sigSrcs :: [(String, Device SigSrc)]
sigSrcs = [] -- ("seconds", secondsSig)]

sigSnks :: [(String, Device SigSnk)]
sigSnks = [("print", printSnk)] 
           --("plot", plotSnk)]


allSrcNames = map fst sigSrcs

run :: Env -> [Declare] ->Double -> Double -> IO ()
run prelude decls dt tmax 
    = do let exprs = concatMap declExprs decls
	 --let initEnvExprs = [(n,e) | Let n e <- decls]
         --let initEnv = [] 
         env <- newIORef (("fixedDt", NumV . NReal $ dt):prelude)
         unsolvd <- newIORef [(n, {-changeSigDelay dt-} e) | Let n e <- decls]
         --unsolvEvts <- newIORef [(n,changeSigDelay dt e) | LetEvt n e <- decls]
         --let (initEnv, unsolvd) =  resolveExprs [] initEnvExprs
         let addEnv n v = readIORef env >>= writeIORef env . ((n,v):)
         --let nmsigs = [ (nm, sigE ) | LetSig nm sigE <- decls ] 
         let readEvalS = do e <- readIORef env
                            return $ EvalS dt tmax Nothing e
         let evalIO e  = do s <- readEvalS 
                            runEvalM (eval s e)
         let tryResolv = do e <- readIORef env
                            u <- readIORef unsolvd
                            -- uevts <- readIORef unsolvEvts
                            let (e',u')= resolveExprs e dt tmax u 
                            writeIORef env e'
                            writeIORef unsolvd u'
                            -- writeIORef unsolvEvts uev'
         
         tryResolv
	 -- find out which srcs are needed, init them
	 let srcNeeded = extractSigs exprs
         let srcs = lookupMany srcNeeded sigSrcs 
         mapM_ (prepareWith . snd) srcs 
	 --print $ map fst srcs

         -- find out which sinks are needed, init them
         let snks' = concatMap declSinks decls
         let snks = map (\(snm,e)->(fromJust $ lookup snm sigSnks ,e)) snks'
         let (snksNow, snksLater) = partition (loadBefore . fst) snks
         mapM_ (prepareWith . fst) snksNow 
	 --putStrLn $ "snks now="++(show $ map snd snksNow)	 
        -- putStrLn $ "snks later="++(show $ map snd snksLater)


	 -- put all the outputs

	 forM_ snksNow $ \(s,e) -> do 
           SigV sigf <- evalIO e
           let vls = map (\t->(t, sigf t)) [0,dt..tmax]
           applyVlToSnk vls s
	 -- trigger

         --get all the inputs
         forM_ srcs $ \(n,srcDev) -> do
           sig <- getVlsFromSig srcDev
           addEnv n sig
           return ()

         --done trial

         mapM_ (flushWith . snd) srcs 
         mapM_ (flushWith . fst) snks 

	 --run the rest of the network
         --find all sig exprs in decls

         -- print nmsigs

         --dumpEnv env "before resolve"
         --dumpEnv unsolvd "unsolved"
         tryResolv
         --dumpEnv env "after resolve"
         --rest of sinks
         envNow <- readIORef env
	 forM_ snksLater $ \(s,e) -> do
		 (SigV sig) <- evalIO e -- case eval envNow e Nothing of
                 let tms = [0,dt..tmax]
                 let vals = zip tms $ map sig tms
                 applyVlToSnk vals s
                 
         mapM (\(n,v) -> putStrLn (n++" = "++show v) ) $ remPrelude prelude envNow 
	 return () 
             where dumpEnv e s = do putStrLn $ "Env("++s++")={" 
                                    readIORef e >>= mapM_ (\(k,v)-> do putStr (show k) 
                                                                       putStr " := " 
                                                                       print v)
                                    putStrLn "}" 

remPrelude :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
remPrelude prel env = filter (\(x,y)-> not (x `elem` map fst prel)) env

resolveExprs :: Env -> Double -> Double ->[(String,E)] -> (Env, [(String,E)])
resolveExprs env dt tmax newSigs 
	 = if null newSolvd --  && null solvdEvts
              then ( env, solvLater)
              else resolveExprs (newSolvd++env) dt tmax solvLater
    where (newSolvd,solvLater) = partitionEithers $ map maybeSolve newSigs
          -- (solvdEvts,evtsLater) = partitionEithers $ map maybeSolveEvts newEvts
          maybeSolve :: (String,E) -> Either (String,V) (String,E)
          maybeSolve (n, vs) = if haveAllVarsInExpr n vs
                                   then evalExpr (n,vs)
                                   else {- ulog (concat ["missing vars for ",
                                                     n, ": ",
                                                     show (freeVars vs)]) $ -} Right (n,vs)
          haveAllVarsInExpr n e = all (`elem` (n:map fst env)) $ freeVars e
          fromRight (Right x) = x
          es :: EvalS
          es = EvalS dt tmax Nothing env
          evalExpr :: (String,E)->Either (String,V) (String,E)
          evalExpr (n,ss) = case  eres of
                                Right v -> Left (n,v)
                                Left x ->ulog (show ss++"\n"++show x) $ Right (n,ss)
              where eres =  sfEvalM (eval (extEnv (n, fromRight eres) es) ss)
                    nres = sfEvalM (eval es ss)


ulog x y = unsafePerformIO (putStrLn ("unsafe log: "++x) >> return y)

lookupMany :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupMany nms assoc 
    = justSnds $ map (\nm->(nm, lookup nm assoc)) nms 


	{-= do 	e <- newIORef []
		forM_ [0,dt..tmax] $ \t-> print t
-}

justSnds :: [(a,Maybe b)]->[(a,b)]
justSnds [] = []
justSnds ((x,Nothing):rest) = justSnds rest
justSnds ((x,Just y):rest) = (x,y):(justSnds rest)

declExprs (Let _ e) = [e]
--declExprs (LetSig _ e) = [e]
declExprs (LetRec _ e) = [e]
declExprs (SinkConnect e _) = [e]
-- declExprs (LetEvt _ e) = [e]

declSinks (SinkConnect e s) = [(s,e)] 
declSinks _ = []

extractSigs :: [E] -> [String]
extractSigs exprs = concatMap (queryE q) exprs
	where 	q (Var nm) | nm `elem` allSrcNames = [nm]
		q _ = []

--in base 4
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers es = (lefts es, rights es )

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x:es) =x:lefts es
lefts (_:es) = lefts es

rights :: [Either a b] -> [b]
rights [] = []
rights (Right x:es) =x:rights es
rights (_:es) = rights es


changeSigDelay :: Double -> E -> E
changeSigDelay dt es = (mapE f es)
    where f (SigDelay s p0) = Sig $ SigAt (M2 Sub time dt') s
          f e =e 
          dt' = (Const . NumV $ NReal dt)
          time = SigVal (Var "seconds")