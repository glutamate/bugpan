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
--import Stages
import Data.Ord
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
  sesns <- getSessionInRootDir rootDir
  sesnsTm <- mapM (\dirNm-> do tm <- getModificationTime (oneTrailingSlash rootDir++dirNm)
                               return (oneTrailingSlash rootDir++dirNm, tm)) sesns
  let dir = fst $ maximumBy (comparing snd) sesnsTm
  loadExactSession dir

loadExactSession :: FilePath -> IO Session
loadExactSession dir = do
  t0 <- read `fmap` readFile (dir++"/tStart")
  return $ Session dir (TOD (fst t0) (snd t0))
    

getSessionInRootDir rootDir = do
  sesns <- getDirContents rootDir
  --mapM print sesns
  filterM (\objNm -> do isDir <- doesDirectoryExist $ oneTrailingSlash rootDir++objNm
                        return $ isDir) sesns

loadSession :: FilePath -> String -> IO Session
loadSession rootDir initnm = do
  sesns <- (filter (initnm `isPrefixOf`)) `fmap` getSessionInRootDir rootDir
  case sesns of 
    (sess:[]) -> loadExactSession $ oneTrailingSlash rootDir++sess
    [] -> fail $ "No session starting with "++initnm++" in "++rootDir
    ss -> fail $ "Ambiguous session "++initnm ++": "++show ss
  

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
          t1 = NumV. NReal $ t0
          t2 = NumV. NReal $ t0+tmax
          tStartEvs = [("tStart", ListV [PairV t1 Unit]),
                       ("tStop", ListV [PairV t2 Unit])]
          progEp = ("program", ListV [PairV (PairV t1 t2) (StringV (unlines $ map ppDecl decls))])
          saveInSubDir subdir nm obj = do
            let dir = (basedir++"/"++subdir++"/"++nm)
            createDirectoryIfMissing False dir
            let ntics = round $ t0/dt
            saveBinary (dir++"/"++showHex ntics "") obj
      in do -- Session newEvs newSigSegs newEps ((t0,t0+tmax, decls):programsRun sess) (qenv sess) (sessPrelude sess)
        print "saving sessopm"
        forM sigsToStore $ \(nm,sig) -> do
          putStrLn $"saving "++ nm
          saveInSubDir "signals" nm sig
          putStrLn "done"
        forM (tStartEvs++evtsToStore) $ \(nm, ListV evs) -> do
          saveInSubDir "events" nm evs
        forM (progEp:epsToStore) $ \(nm, ListV eps) -> do
          saveInSubDir "epochs" nm eps
        print "done saving sessopm"
        return ()


saveBinary :: B.Binary w => FilePath-> w -> IO ()
saveBinary fp w = L.writeFile fp {-. compress-} . B.encode $ w --writeFile fp . show

appendBinary :: B.Binary w => FilePath-> w -> IO ()
appendBinary fp w = L.appendFile fp {-. compress-} . B.encode $ w --writeFile fp . show

loadBinary :: B.Binary w =>FilePath-> IO w
loadBinary fp = return . B.decode {-. decompress -}=<< L.readFile fp --readFile fp >>= return . read


for = flip map

isTrue (BoolV True) = True

isNotFalse (BoolV False) = False
isNotFalse _ = True

sigInEps s@(SigV ts1 ts2 dt sf) eps = 
    catMaybes $ for eps $ \ep-> let (tep1,tep2) = epTs ep in
                                cond [(ts1<tep1 && ts2>tep2, Just $ SigV tep1 tep2 dt $ \t->sf(t-(round $ tep1/dt)))]

evTime (PairV (NumV (NReal t)) _) = t
evTag (PairV (NumV (NReal t)) v) = v

epTs (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) _) = (t1,t2)
epTag (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) = v

isEpoch (PairV (PairV (NumV (NReal _)) ((NumV (NReal _)))) _) = True
isEpoch _ = False

isEvent (PairV (NumV (NReal _)) _) = True
isEvent _ = False

isEvents (ListV vs) = all isEvent vs
isEvents _ = False

isEpochs (ListV vs) = all isEpoch vs
isEpochs _ = False


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


shiftSig ts (SigV t1 t2 dt sf) = SigV (t1+ts) (t2+ts) dt $ \t->sf(t-(round $ ts/dt))
shiftEvt ts (PairV (NumV (NReal t)) v) = (PairV (NumV (NReal $ t+ts)) v)
shiftEp ts (PairV (PairV (NumV (NReal t1)) ((NumV (NReal t2)))) v) = 
    (PairV (PairV (NumV (NReal $t1+ts)) ((NumV (NReal $t2+ts)))) v)

mkList :: V -> [V]
mkList (ListV vs) = vs
mkList v = [v]



getDirContents dir = do objs <- liftIO $ getDirectoryContents dir
                        return $ filter (not . (`elem` [".", ".."])) objs
