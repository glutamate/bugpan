{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Database where

import EvalM hiding (ListT)
import Eval
import Expr
import Data.Maybe
import Data.List
import Numbers
{-import ImpInterpret
import Compiler 
import Stages
import Traverse
import Transform-}
import Control.Monad
import Control.Monad.List
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Random
import System.Info.MAC as MAC
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal
import qualified Data.Binary as B

data Session = Session { baseDir :: FilePath
                       } deriving (Eq, Show)

asInt :: Int -> Int
asInt = id


oneTrailingSlash "/" = "/"
oneTrailingSlash "" = ""
oneTrailingSlash s = case last s of
                      '/' -> s
                      _ -> s++"/"

newSession :: FilePath -> IO Session
newSession rootDir = do
  TOD t1 t2 <- getClockTime
  Just mac <- MAC.new
  rnd <- asInt `fmap` randomIO
  let longStr = concat [show t1, show t2, show mac, show rnd] 
  --putStrLn longStr
  let sha = take 20 . showDigest . sha512 . BS.pack $ map c2w "foo"
  let baseDir = oneTrailingSlash rootDir++sha
  --print baseDir
  createDirectory baseDir
  createDirectory $ baseDir++"/signals"
  createDirectory $ baseDir++"/events"
  createDirectory $ baseDir++"/epochs"
  return $ Session baseDir
--sessEvalState s = EvalS 0 0 Nothing (qenv s ++( evalManyAtOnce $ sessPrelude s))

addRunToSession :: [Declare] -> Double -> Double -> [(String, V)] -> Session -> IO ()
addRunToSession decls t0 tmax ress sess 
    = let nmsToStore = [ nm | SinkConnect (Var nm) "store" <- decls ]
          sigsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup ('#':nm) ress of
                                Just (SigV t1 t2 sf) -> Just (nm, [(t1+t0,t2+t0, \t->sf (t-t0))])
                                _ -> Nothing
          evtsToStore = reverse . catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress `guardBy` isEvents of
                                Just (ListV evs) -> 
                                    Just (nm, map (\(PairV (NumV (NReal tm)) v)-> (tm+t0,v)) evs)
                                _ -> 
                                    Nothing
          epsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress `guardBy` isEpochs of
                                Just (ListV eps) -> 
                                    Just (nm, map (\(PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) ->
                                                         (t1+t0, t2+t0, v)) eps)
                                _ -> 
                                    Nothing 
          --newEvs = spliceAssocs evtsToStore (events sess)
          --newSigSegs = spliceAssocs sigsToStore (sigSegments sess)
          --newEps = spliceAssocs epsToStore (epochs sess)  
      in do -- Session newEvs newSigSegs newEps ((t0,t0+tmax, decls):programsRun sess) (qenv sess) (sessPrelude sess)
        

        return ()

instance B.Binary V where
    put (BoolV b) = B.put b
    get _ = undefined

data AsBool = AsBool V



{-
spliceAssocs :: Eq a => [(a,[b])] ->[(a,[b])]->[(a,[b])]
spliceAssocs = spliceAssocs' []
spliceAssocs' accum assoc1 [] =accum++assoc1
spliceAssocs' accum assoc1 ((key,vls):assoc2) 
    = case lookup key assoc1 of
        Just moreVls -> spliceAssocs' ((key,vls++moreVls):accum) 
                                      (filter ((/=key) . fst) assoc1) 
                                      (assoc2) 
                        
        Nothing -> spliceAssocs' ((key,vls):accum) assoc1 (assoc2) 

runOnce :: Double -> Double -> Double -> [Declare] -> Session -> IO Session
runOnce dt t0 tmax ds sess = do
  --let prel = map (\(n,v)->(n,Const v)) (sessPrelude sess)
  let runTM = runTravM ds $ sessPrelude sess
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude
  ress <- execInStages (complPrel++prg) dt tmax
  putStrLn $ "results for this trial: "++show ress
  let nsess = addRunToSession ds t0 tmax ress sess
  return nsess

evalManyAtOnce :: [(String, E)] -> [(String, V)]
evalManyAtOnce es = 
    let env = map (\(n,e)->(n,unEvalM $ eval (evalS env) e)) es
    in env

runNtimes :: Int -> Double -> Double -> Double -> [Declare] -> [(String, E)] -> IO Session

runNtimes n dt tmax tsep ds prel = 
  let sess = emptySession {sessPrelude = prel} in
  runNtimes' n dt tmax tsep ds sess
  --print sess >> return sess

runNtimes' 0 _ _ _ _ sess = return sess
runNtimes' n dt tmax tsep ds sess = do
  let tstart = case programsRun sess of 
                        [] -> 0
                        ((t1,t2,_):_) -> t1+tsep
  runOnce dt tstart tmax ds sess >>=
      runNtimes' (n-1) dt tmax tsep ds


prevTrialStart sess = case programsRun sess of 
                        [] -> 0
                        ((t1,t2,_):_) -> t1
  --return sess

-}

for = flip map

isTrue (BoolV True) = True

isNotFalse (BoolV False) = False
isNotFalse _ = True

sigInEps s@(SigV ts1 ts2 sf) eps = 
    catMaybes $ for eps $ \ep-> let (tep1,tep2) = epTs ep in
                                cond [(ts1<tep1 && ts2>tep2, Just $ SigV tep1 tep2 $ \t->sf(t-tep1))]

evTime (PairV (NumV (NReal t)) _) = t

epTs (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) _) = (t1,t2)

isEpochs (ListV ((PairV (PairV (NumV (NReal _)) ((NumV (NReal _)))) _):_)) = True
isEpochs _ = False

isEvents (ListV ((PairV (NumV (NReal _)) _):_)) = True
isEvents _ = False

--guardBy :: (MonadPlus m) => m a -> (a->Bool) -> m a
guardBy :: Maybe a -> (a->Bool) -> Maybe a
guardBy Nothing _ = Nothing
guardBy (Just x) p | p x = Just x
                   | otherwise = Nothing
{- do x <- mx
                  if p x
                     then mx
                     else Nothing -}


shiftSig (SigV t1 t2 sf) ts = SigV (t1+ts) (t2+ts) $ \t->sf(t-ts)

mkList :: V -> [V]
mkList (ListV vs) = vs
mkList v = [v]

evInEpoch ev ep = let (t1, t2) = epTs ep 
                      tev = evTime ev
                  in tev<t2 && tev>t1



{-
newtype AskM a = AskM { unAskM :: ListT (StateT Session IO) a }
    deriving (Monad, MonadIO, Functor, MonadState Session, MonadPlus)

runAskM :: Session -> AskM a -> IO [a]
runAskM sess (AskM lsioA) = fst `fmap` runStateT (runListT (lsioA)) sess

answers :: [a] -> AskM a
answers xs = AskM (ListT . return $ xs)
answer x = AskM (ListT . return $ [x])

askM :: Q -> AskM V
askM (QVar nm) = do
  sess <- get
  case lookup nm $ qenv sess of
        Just (ListV vs) -> answers vs
        Just v -> answer v
        Nothing -> lookupInSigs
            where lookupInSigs =  
                      case lookup nm $ sigSegments sess of
                        Just vls -> answers $ map (\(t1,t2,sf)-> SigV t1 t2 sf) vls
                        Nothing -> lookupInEvts
                  lookupInEvts = 
                      case lookup nm $ events sess of
                        Just vls -> answers $ map (\(t,v)-> PairV (NumV . NReal $ t) (v)) vls
                        Nothing -> answers []

askM (Map lame q) = do
  es <- sessEvalState `fmap` get
  let f v = unEvalM $ eval es (App lame (Const v))
  f `fmap` askM q

askM (Filter pred q) = do
  es <- sessEvalState `fmap` get
  let f v = unEvalM $ eval es (App pred (Const v))
  vs <- askM q
  guard (isNotFalse $ f vs)
  return vs

askM (Has qep qevs) = do 
  ev <- askM qevs
  ep <- askM qep
  guard (ev `evInEpoch` ep)
  return ep
                              
                             -}
data Q = QVar String
       -- | Filter E Q
       -- | Map E Q
       | Filter E Q
       | Map E Q
       | Has Q Q
       | In Q Q
       | Around Q Q

