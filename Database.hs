{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSignatures #-} 

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
--import System.Random
--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L
--import Data.ByteString.Internal
import qualified Data.Binary as B
import Data.UUID
import Data.UUID.V1
import Numeric
import Traverse
import Transform
import Stages
import Data.Ord
import Charts
import Control.Concurrent


data Session = Session { baseDir :: FilePath,
                         tSessionStart :: ClockTime
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
  t0@(TOD t1 t2) <- getClockTime  
  --Just mac <- MAC.new
  ---rnd <- asInt `fmap` randomIO
  --let longStr = concat [show t1, show t2, show mac, show rnd] 
  --putStrLn longStr
  --let sha = take 20 . showDigest . sha512 . BS.pack $ map c2w "foo"
  Just uuid <- (fmap (filter (/='-') . toString)) `fmap` nextUUID
  let baseDir = oneTrailingSlash rootDir++ uuid
  --print baseDir
  createDirectory baseDir
  createDirectory $ baseDir++"/signals"
  createDirectory $ baseDir++"/events"
  createDirectory $ baseDir++"/epochs"
  writeFile (baseDir++"/tStart") $ show (t1, t2)
  return $ Session baseDir t0
--sessEvalState s = EvalS 0 0 Nothing (qenv s ++( evalManyAtOnce $ sessPrelude s))

lastSession :: FilePath -> IO Session
lastSession rootDir = do
  sesns <- getDirContents rootDir
  --mapM print sesns
  sesns <- filterM (\objNm -> do isDir <- doesDirectoryExist $ oneTrailingSlash rootDir++objNm
                                 return $ isDir) sesns
  sesnsTm <- mapM (\dirNm-> do tm <- getModificationTime (oneTrailingSlash rootDir++dirNm)
                               return (oneTrailingSlash rootDir++dirNm, tm)) sesns
  let dir = fst $ maximumBy (comparing snd) sesnsTm
  t0 <- read `fmap` readFile (dir++"/tStart")
  return $ Session dir (TOD (fst t0) (snd t0))

addRunToSession :: [Declare] -> Double -> Double -> Double -> [(String, V)] -> Session -> IO ()
addRunToSession decls t0 tmax dt ress sess@(Session basedir sesst0) 
    = let nmsToStore = [ nm | SinkConnect (Var nm) "store" <- decls ]
          sigsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup ('#':nm) ress `guardBy` isSig of
                                Just s@(SigV t1 t2 _ sf) -> Just (nm,shiftSig t0 s)
                                _ -> Nothing
          evtsToStore = reverse . catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress `guardBy` isEvents of
                                Just (ListV evs) -> Just (nm, ListV . reverse $ map (shiftEvt t0) evs)
                                    
                                _ -> 
                                    Nothing
          epsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \nm-> case lookup nm ress `guardBy` isEpochs of
                                Just (ListV eps) -> Just (nm,ListV $ map (shiftEp t0) eps)
                                _ -> Nothing           
          saveInSubDir subdir nm obj = do
            let dir = (basedir++"/"++subdir++"/"++nm)
            createDirectoryIfMissing False dir
            let ntics = round $ t0/dt
            saveBinary (dir++"/"++showHex ntics "") obj
      in do -- Session newEvs newSigSegs newEps ((t0,t0+tmax, decls):programsRun sess) (qenv sess) (sessPrelude sess)
        forM sigsToStore $ \(nm,sig) -> do
          saveInSubDir "signals" nm sig
        forM evtsToStore $ \(nm, ListV evs) -> do
          saveInSubDir "events" nm evs
        forM epsToStore $ \(nm, ListV eps) -> do
          saveInSubDir "epochs" nm eps
        return ()


saveBinary :: B.Binary w => FilePath-> w -> IO ()
saveBinary fp w = L.writeFile fp {-. compress-} . B.encode $ w --writeFile fp . show

appendBinary :: B.Binary w => FilePath-> w -> IO ()
appendBinary fp w = L.appendFile fp {-. compress-} . B.encode $ w --writeFile fp . show

loadBinary :: B.Binary w =>FilePath-> IO w
loadBinary fp = return . B.decode {-. decompress -}=<< L.readFile fp --readFile fp >>= return . read


runOnce :: Double -> Double -> Double -> [Declare] -> [(String, E)] -> Session -> IO ()
runOnce dt t0 tmax ds prel sess = do
  --let prel = map (\(n,v)->(n,Const v)) (sessPrelude sess)
  let runTM = runTravM ds prel
  let prg = snd . runTM $ transform
  let complPrel =  fst . runTM $ compilablePrelude
  ress <- execInStages (complPrel++prg) dt tmax return
  putStrLn $ "results for this trial: "++show ress
  addRunToSession ds t0 tmax dt ress sess
  return ()


runNtimes :: Int -> Double -> Double -> Double -> Double -> [Declare] -> [(String, E)] -> Session -> IO ()
runNtimes 0 _   _    _      _    _  _   _ = return ()
runNtimes n dt tmax tstart tsep ds prel sess = do
  runOnce dt tstart tmax ds prel sess 
  runNtimes (n-1) dt tmax (tstart+tsep+tmax) tsep ds prel sess


for = flip map

isTrue (BoolV True) = True

isNotFalse (BoolV False) = False
isNotFalse _ = True

sigInEps s@(SigV ts1 ts2 dt sf) eps = 
    catMaybes $ for eps $ \ep-> let (tep1,tep2) = epTs ep in
                                cond [(ts1<tep1 && ts2>tep2, Just $ SigV tep1 tep2 dt $ \t->sf(t-tep1))]

evTime (PairV (NumV (NReal t)) _) = t

epTs (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) _) = (t1,t2)

isEpochs (ListV ((PairV (PairV (NumV (NReal _)) ((NumV (NReal _)))) _):_)) = True
isEpochs _ = False

isEvents (ListV ((PairV (NumV (NReal _)) _):_)) = True
isEvents _ = False

isSig (SigV _ _ _ _) = True
isSig _ = False


--guardBy :: (MonadPlus m) => m a -> (a->Bool) -> m a
guardBy :: Maybe a -> (a->Bool) -> Maybe a
guardBy Nothing _ = Nothing
guardBy (Just x) p | p x = Just x
                   | otherwise = Nothing
{- do x <- mx
                  if p x
                     then mx
                     else Nothing -}


shiftSig ts (SigV t1 t2 dt sf) = SigV (t1+ts) (t2+ts) dt $ \t->sf(t-ts)
shiftEvt ts (PairV (NumV (NReal t)) v) = (PairV (NumV (NReal $ t+ts)) v)
shiftEp ts (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) = 
    (PairV (PairV (NumV (NReal $t1+ts)) ((NumV (NReal $t2+ts)))) v)

mkList :: V -> [V]
mkList (ListV vs) = vs
mkList v = [v]

evInEpoch ev ep = let (t1, t2) = epTs ep 
                      tev = evTime ev
                  in tev<t2 && tev>t1

getDirContents dir = do objs <- liftIO $ getDirectoryContents dir
                        return $ filter (not . (`elem` [".", ".."])) objs

getSortedDirContents dir = do conts <- getDirContents dir
                              let sconts = sortBy cmpf conts
                              liftIO $ print sconts
                              return sconts
    where cmpf f1 f2 = case (readsPrec 5 f1, readsPrec 5 f2) of
                         ((n1::Int,_):_, (n2::Int,_):_) -> compare n1 n2
                         _ -> EQ



newtype AskM a = AskM { unAskM :: ListT (StateT Session IO) a }
    deriving (Monad, MonadIO, Functor, MonadState Session, MonadPlus)

runAskM :: Session -> AskM a -> IO [a]
runAskM sess (AskM lsioA) = fst `fmap` runStateT (runListT (lsioA)) sess

answers :: [a] -> AskM a
answers xs = AskM (ListT . return $ xs)
answer x = AskM (ListT . return $ [x])

signals :: String -> AskM V
signals nm = do
  Session bdir t0 <- get
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/signals/"++nm++"/"++fn
          answers sigs)
      (answers [])

events :: String -> AskM V
events nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          evs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/events/"++nm++"/"++fn
          answers (concat evs))
      (answers [])

epochs :: String -> AskM V
epochs nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/epochs/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/epochs/"++nm
          evs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/epochs/"++nm++"/"++fn
          answers evs)
      (answers [])

askM :: Q -> AskM V
askM (Map lame q) = do
  let f v = unEvalM $ eval emptyEvalS (App lame (Const v))
  f `fmap` askM q

askM (Filter pred q) = do
  let f v = unEvalM $ eval emptyEvalS (App pred (Const v))
  vs <- askM q
  guard (isNotFalse $ f vs)
  return vs

askM (Has qep qevs) = do 
  ev <- askM qevs
  ep <- askM qep
  guard (ev `evInEpoch` ep)
  return ep
            

plot :: [V] -> IO ()
plot vs = do --let g = map ansToPlot ans
             plotGraph (valsToGraph vs)
             return ()
          

valsToGraph :: [V] -> Graph
valsToGraph vs = foldl1 (<+>) $ map vToPlot vs
    where vToPlot (SigV t1 t2 dt sf)= toGraph ((toPlot $map (\t -> (t, unsafeVToDbl $ sf t)) [t1, t1+dt..t2])%Lines)

data Q = QVar String
       -- | Filter E Q
       -- | Map E Q
       | Filter E Q
       | Map E Q
       | Has Q Q
       | In Q Q
       | Around Q Q

