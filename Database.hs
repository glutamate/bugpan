{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Database where

import EvalM
--import Eval
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
--import Control.Monad.List
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Cmd
--import System.Random
--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
--import Data.ByteString.Internal
import Data.UUID
import Data.UUID.V1
import Numeric
--import Traverse
--import Transform
--import Stages
import Data.Ord
import Control.Concurrent
import TNUtils
import PrettyPrint
import ValueIO

data Session = Session { baseDir :: FilePath,
                         tSessionStart :: ClockTime
                       } deriving (Eq, Show)

withoutTrailing c [] = []
withoutTrailing c cs = if last cs == c
                          then init cs
                          else cs


createSession rootDir t0@(TOD t1 t2) name = do
  let baseDir = rootDir ./ name
  --print baseDir
  createDirectory baseDir
  createDirectory $ baseDir./ "signals"
  createDirectory $ baseDir./ "events"
  createDirectory $ baseDir./ "durations"
  writeFile (baseDir./ "tStart") $ show (t1, t2)
  writeFile (baseDir./ "sessionFormatVersion") $ "3"
  return $ Session baseDir t0

newSession :: FilePath -> IO Session
newSession rootDir = do
  t0@(TOD t1 t2) <- getClockTime  
  --Just mac <- MAC.new
  ---rnd <- asInt `fmap` randomIO
  --let longStr = concat [show t1, show t2, show mac, show rnd] 
  --putStrLn longStr
  --let sha = take 20 . showDigest . sha512 . BS.pack $ map c2w "foo"
  muuid <- (fmap (filter (/='-') . toString)) `fmap` nextUUID
  print muuid
  Just uuid <- return muuid
  createSession rootDir t0 uuid
--sessEvalState s = EvalS 0 0 Nothing (qenv s ++( evalManyAtOnce $ sessPrelude s))

cloneSession :: Session -> String-> Int -> IO Session
cloneSession (Session oldBasedir t0@(TOD t1 t2)) postfix newVersion = do
  let baseDir = withoutTrailing '/' oldBasedir ++ postfix
  createDirectory baseDir
  createDirectory $ baseDir ./ "signals"
  createDirectory $ baseDir ./ "events"
  createDirectory $ baseDir ./ "durations"
  writeFile (baseDir ./ "tStart") $ show (t1, t2)
  writeFile (baseDir ./ "sessionFormatVersion") $ show newVersion
  return $ Session baseDir t0
--sessEvalState s = EvalS 0 0 Nothing (qenv s ++( evalManyAtOnce $ sessPrelude s))


sessionTypes :: Session ->  IO [(String, T)]
sessionTypes sess@(Session dir' _) = do
  let dir = oneTrailingSlash dir'
  xs <- forM ["signals", "events", "durations"] $ \kind -> do
                                       sigs <- getDirContents $ dir ./ kind
                                       --print sigs
                                       forM sigs $ \sig -> do 
                                         --print (kind, sig)
                                         do fnms <- getSortedDirContents $ dir ./ kind ./ sig
                                            fTT <- fileTypeTag $ dir ./ kind ./ sig ./ (head fnms)
                                         --v<-loadUntyped $ dir./  kind./  sig
                                         --print (kind, sig, head v )
                                            return (sig, fTT) --typeOfVal $ head v)
  return $ concat xs

loadUntyped :: FilePath -> IO [V]
loadUntyped fp = do 
  ifM (doesDirectoryExist fp)
      (do fnms <- getSortedDirContents fp
          xs <- forM fnms $ \fn-> loadVs $ fp./  fn
          return $ concat xs)
      ((print $ "dir not found:" ++fp) >> return [])

lastSession :: FilePath -> IO Session
lastSession rootDir = do
  sesns <- getSessionInRootDir rootDir
  sesnsTm <- mapM (\dirNm-> do tm <- getModificationTime (oneTrailingSlash rootDir++dirNm)
                               return (oneTrailingSlash rootDir++dirNm, tm)) sesns
  let dir = fst $ maximumBy (comparing snd) sesnsTm
  loadExactSession dir

deleteSession :: Session -> IO ()
deleteSession (Session dir _) = system ("rm -rf "++ dir) >> return ()
   

resolveApproxSession  :: FilePath -> String -> IO String
resolveApproxSession  root nm = do
  sessns <- getSessionInRootDir root
  --print sessns
  case find (nm `isPrefixOf`) sessns of
    Just s -> return s
    _ -> fail $ "resolveApproxSession: cannot find session "++nm++" in "++root
  
loadApproxSession :: FilePath -> String -> IO Session
loadApproxSession root nm = do
  sessNm <- resolveApproxSession root nm
  loadExactSession $ oneTrailingSlash root++sessNm

loadExactSession :: FilePath -> IO Session
loadExactSession dir = do
  t0 <- read `fmap` readFile (dir ./ "tStart")
  return $ Session dir (TOD (fst t0) (snd t0))

getSessionInRootDir rootDir = do
  sesns <- getDirContents rootDir
  --mapM print sesns
  filterM (\objNm -> do isDir <- doesDirectoryExist $ rootDir ./ objNm
                        return $ isDir) sesns

getSortedDirContents dir = do conts <- getDirContents dir
                              let sconts = sortBy cmpf conts
                              --liftIO $ print sconts
                              return sconts
    where cmpf f1 f2 = case (readHex f1, readHex f2) of
                         ((n1,_):_, (n2,_):_) -> compare (idInt n1) (idInt n2)
                         _ -> EQ


loadSession :: FilePath -> String -> IO Session
loadSession rootDir initnm = do
  sesns <- (filter (initnm `isPrefixOf`)) `fmap` getSessionInRootDir rootDir
  case sesns of 
    (sess:[]) -> loadExactSession $ oneTrailingSlash rootDir++sess
    [] -> fail $ "No session starting with "++initnm++" in "++rootDir
    ss -> fail $ "Ambiguous session "++initnm ++": "++show ss
  
orIfEmpty :: [a] -> [a] -> [a]
orIfEmpty [] xs = xs
orIfEmpty xs _ = xs

unString :: E -> String
unString (Const (StringV s)) = s
unString _ = ""

addRunToSession :: [Declare] -> RealNum -> RealNum -> RealNum -> [(String, V)] -> Session -> IO ()
addRunToSession decls t0 tmax dt ress sess@(Session basedir sesst0) 
    = let nmsToStore = [ (nm, unString arg `orIfEmpty` nm) | SinkConnect (Var nm) ("store",arg) <- decls ]
          sigsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \(nm, nmStore)-> case lookup ('#':nm) ress `guardBy` isSig of
                                Just s@(SigV _ _ _ _) -> Just (nmStore,shift t0 s)
                                _ -> Nothing
          evtsToStore = reverse . catMaybes . 
                        flip map nmsToStore $ 
                        \(nm, nmStore) -> case lookup nm ress `guardBy` isEvents of
                                Just (ListV evs) -> Just (nmStore,  reverse $ map (shift t0) evs)
                                    
                                _ -> 
                                    Nothing
          epsToStore = catMaybes . 
                        flip map nmsToStore $ 
                        \(nm, nmStore) -> case lookup nm ress `guardBy` isEpochs of
                                Just (ListV eps) -> Just (nmStore, map (shift t0) eps)
                                _ -> Nothing
          t1 = NumV. NReal $ t0
          t2 = NumV. NReal $ t0+tmax
          moduleName = safeHead [nm | Let "moduleName" (Const (StringV nm)) <- decls]
          moduleEps = case moduleName of
                        Nothing -> []
                        Just nm -> [("moduleName", [PairV (PairV t1 t2) (StringV (nm))]),
                                    (nm, [PairV (PairV t1 t2) Unit])]
          tStartEvs = [("tStart",  [PairV t1 Unit]),
                       ("tStop",  [PairV t2 Unit])]
          progEp = [("program",  [PairV (PairV t1 t2) (StringV (unlines $ map ppDecl decls))]),
                    ("running",  [PairV (PairV t1 t2) Unit])]++moduleEps
                    
          saveInSubDir subdir nm obj = do
            let dir = (basedir ./ subdir ./ nm)
            createDirectoryIfMissing False dir
            let ntics = round $ t0/dt
            saveVs (dir ./ showHex ntics "") obj
      in do -- Session newEvs newSigSegs newEps ((t0,t0+tmax, decls):programsRun sess) (qenv sess) (sessPrelude sess)
        putStrLn $ "saving session: "++show nmsToStore
        --putStrLn $ "from results: "++ show ress
        forM sigsToStore $ \(nm,sig) -> do
          putStrLn $"saving signal "++ nm++": "++ show sig
          saveInSubDir "signals" nm [sig]
          putStrLn "done"
        forM (tStartEvs++evtsToStore) $ \(nm, evs) -> do
	  putStrLn $"saving events "++ nm
          saveInSubDir "events" nm evs
        forM (progEp++epsToStore) $ \(nm, eps) -> do
	  putStrLn $"saving epochs "++ nm
          saveInSubDir "durations" nm eps
        print "done saving session"
        return ()


--simpler interface
saveInSession sess@(Session basedir _) nm t0 dt sig@(SigV t1 t2 sigdt sf) = do
  let dir = basedir ./ "signals/"++nm
  createDirectoryIfMissing False dir
  let ntics = round $ t0/dt
  saveVs (dir./  showHex ntics "") $ [shift t0 sig]
saveInSession sess@(Session basedir sesst0) nm t0 dt lst@(ListV evs) | isEvents lst = do
  let dir = basedir ./ "events/"++nm
  createDirectoryIfMissing False dir
  let ntics = round $ t0/dt
  saveVs (dir./  showHex ntics "") . reverse $ map (shift t0) evs
                                                                     | isEpochs lst = do 
  let dir = basedir ./ "durations/"++nm
  createDirectoryIfMissing False dir
  let ntics = round $ t0/dt
  saveVs (dir ./ showHex ntics "") $ map (shift t0) evs

isTrue (BoolV True) = True

isNotFalse (BoolV False) = False
isNotFalse _ = True

sigInDurs s@(SigV ts1 ts2 dt sf) durs = 
    catMaybes $ for durs $ \dur-> let (tep1,tep2) = epTs dur in
                                  cond [(ts1<tep1 && ts2>tep2, Just $ SigV tep1 tep2 dt $ \t->sf(t-(round $ tep1/dt)))] 
                                         Nothing

evTime (PairV nv@(NumV _) _) = unsafeVToDbl nv

evTag (PairV (NumV (NReal t)) v) = v

epTs (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) _) = (t1,t2)
epTag (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) = v

isEpoch (PairV (PairV (NumV _) ((NumV _))) _) = True
isEpoch _ = False

isEvent (PairV (NumV _) _) = True
isEvent _ = False

isEvents (ListV vs) = all isEvent vs
isEvents _ = False

isEpochs (ListV vs) = all isEpoch vs
isEpochs _ = False


isSig (SigV _ _ _ _) = True
isSig _ = False

startTime (PairV nv@(NumV _) _) = unsafeVToDbl nv
startTime (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) = t1
startTime (SigV t1 _ _ _) = t1

sortVs :: [V] -> [V]
sortVs [] = []
sortVs vs = sortBy (comparing startTime) vs


--guardBy :: (MonadPlus m) => m a -> (a->Bool) -> m a
guardBy :: Maybe a -> (a->Bool) -> Maybe a
guardBy Nothing _ = Nothing
guardBy (Just x) p | p x = Just x
                   | otherwise = Nothing
{- do x <- mx
                  if p x
                     then mx
                     else Nothing -}

class Shiftable s where
    shift :: RealNum -> s -> s

instance Shiftable V where
    shift ts (SigV t1 t2 dt sf) = SigV (t1+ts) (t2+ts) dt $ sf
    shift ts (PairV (NumV t) v) = (PairV (NumV $ t+(NReal ts)) v)
    shift ts (PairV (PairV (NumV t1) ((NumV t2))) v) = 
        (PairV (PairV (NumV $ t1 +(NReal ts)) ((NumV $ t2 + (NReal ts)))) v)

mkList :: V -> [V]
mkList (ListV vs) = vs
mkList v = [v]



getDirContents dir = do objs <- liftIO $ getDirectoryContents dir
                        return $ filter (not . (`elem` [".", ".."])) objs
