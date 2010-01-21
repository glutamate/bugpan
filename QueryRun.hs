{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification, TypeOperators #-} 

module QueryRun where

import EvalM
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
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Cmd
import System.Random
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal
import Data.Digest.Pure.SHA

import Numeric
import Traverse
import Transform
import Stages
import Data.Ord
--import Charts
import Control.Concurrent
import Database
import HaskSyntaxUntyped
import QueryTypes
import Stages
import Parse
import TNUtils 
import PrettyPrint
import Query
import HaskellBackend
import ValueIO
import Data.Monoid
import Control.Applicative hiding (Const)
import System.Info

simulatedTime :: MonadIO m => StateT QState m ()
simulatedTime = do qs <- get
                   put $ qs { realTime = False }

useRemoteDriver :: MonadIO m => StateT QState m ()
useRemoteDriver = do Session bdir _ <- getSession
                     qs <- get
                     put $ qs { remoteCmdFile = Just $ bdir ./ "program.bug", 
                                realTime = True}

isRemoteDriver :: MonadIO m => StateT QState m Bool
isRemoteDriver = (isJust . remoteCmdFile) `fmap` get

type CompiledToken = Either (String,Double,[(String, T)]) [Declare]

class Invokable a where
    toToken :: a -> IO CompiledToken

--instance Invokable [Char] where
--    toToken 
--type CompiledToken = (String,Double,[(String, T)]) 

useFile fp params subs = do ds <- makeSubs subs `fmap` use fp
                            ifM (isRemoteDriver)
                                (return $ Right ds)
                                (compile ds params)
                          
compile :: MonadIO m => [Declare] -> [(String, T)] -> StateT QState m CompiledToken
compile ds params = do
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  let commentParams = map (\(nm,ty)-> Comment $ "PARAMETER: "++nm++ppType ty) params
  let sha = take 50 . showDigest . sha512 . L.pack $ map c2w $ show (ds++commentParams)
  let dsTrans = snd $ runTravM ds [] transform
  curDir <- liftIO $ getCurrentDirectory
  --liftIO $ mapM_ (putStrLn . ppDecl) dsTrans
  let exefile = if os =="mingw32"
                   then (bugpanRootDir./"queryCache/"./sha++".exe")
                   else (bugpanRootDir./"queryCache/"./sha)
  liftIO $ createDirectoryIfMissing False (bugpanRootDir./"queryCache/")
  liftIO $ whenM (not `fmap` doesFileExist exefile)
                 (do setCurrentDirectory (bugpanRootDir./"queryCache/")
                     compileToHask (sha++".hs") dt trun dsTrans params
                     system $ "ghc -O2 --make "++sha -- -prof -auto-all
                     return ())
  liftIO $ whenM (not `fmap` doesFileExist exefile) 
             (fail "could not compile")
  --hash declares, look in cache
  liftIO $ setCurrentDirectory curDir
  return $ Left (sha, trun, params)

cyclically :: MonadIO m => [a] -> StateT QState m a
cyclically xs = do
  let n = length xs
  ntrials <- length `fmap` durations "running" ()
  return $ xs !! (ntrials `mod` n)

invoke :: MonadIO m => CompiledToken ->[(String,V)] -> StateT QState m ()
invoke (Left (sha, tmax,pars)) vals= do
  s <- get
  t0 <- getTnow
  rt <- realTime `fmap` get
  let t0' = if t0 < lastTStop s 
               then lastTStop s +1
               else t0 
  Session sessNm _ <- getSession
  let valargs = intercalate " " $ map (ppVal . snd) vals --ideally check ordering
  let cmdStr = bugpanRootDir./"queryCache"./sha++" "++(last $ splitBy '/' sessNm)++" "++show t0' ++" "++valargs
  --liftIO . putStrLn $ cmdStr
  --liftIO $ putStrLn ""
  liftIO $ system $ cmdStr -- ++" +RTS -p"
  put $ s { lastTStart = t0',
            lastTStop = t0' + tmax}
  return ()
invoke (Right ds) vals = do
  let newDs = snd $ runTravM ds [] $ do
                           forM_ vals $ \(nm, vl) -> 
                               alterDefinition nm . const . Const $ vl
  run newDs
                                                     

run :: MonadIO m => [Declare] -> StateT QState m ()
run ds = do
  t0 <- getTnow
  runFrom ds t0

runFrom :: MonadIO m => [Declare] -> Double -> StateT QState m()
runFrom ds t0 = ifM (isRemoteDriver)
                    (runFromRemotely ds t0)
                    (runFromLocally ds t0)

runFromRemotely :: MonadIO m => [Declare] -> Double -> StateT QState m ()
runFromRemotely ds t0 = do 
  s <- get
  --liftIO $ print s
  Just cmdFl <- remoteCmdFile `fmap` get
  args <- shArgs `fmap` get
  s <- get
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  if "-nodaq" `elem` args
     then liftIO $ writeFile cmdFl $ show (noDaqTrans ds)
     else liftIO $ writeFile cmdFl $ show ds -- ppProg "RunProgram" ds
  put $ s { lastTStart = t0, lastTStop = t0 + trun }
  --TStop 


runFromLocally :: MonadIO m => [Declare] -> Double -> StateT QState m ()
runFromLocally ds t0 = do
  s <- get
  sess <- getSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess
  put $ s { lastTStart = t0,
            lastTStop = t0 + trun}

data a := b = a := b

inLast :: (MonadIO m , Reify a) => (String := a) -> StateT QState m ()
inLast (nm := val) = do 
  sess@(Session bdir _) <- getSession
  --running <- unitDurationsStrict "running"
  t1 <- lastTStart `fmap` get
  t2 <- lastTStop `fmap` get
  {-case safeLast $ sortBy (comparing (fst . fst)) running of 
    Nothing -> return ()
    Just ((t1,t2),()) -> -}
  
  liftIO $ saveInSession sess nm t1 0.001 $ pack [((0::Double,t2-t1),val)]
{-createDirectoryIfMissing False $ bdir ./ "durations" ./ nm
              fnm <- uniqueFileInDir "stored" $ bdir ./ "durations" ./ nm
              saveVs fnm [pack ((t1,t2),val)]
              putStrLn $ "inlast "++fnm
              return ()-}

getTnow :: MonadIO m  => StateT QState m Double
getTnow = ifM (realTime `fmap` get)
              (do Session _ t0 <- getSession
                  tnow <- liftIO $ getClockTime
                  return $ diffInS tnow t0)
              (lastTStop `fmap` get)

use :: MonadIO m => String -> m [Declare]
use fnm = liftIO $ fileDecls fnm []

with = flip makeSubs

pause :: MonadIO m => Double -> StateT QState m ()
pause secs= ifM (realTime `fmap` get)
                (do s <- get
                    Session _ t0 <- getSession
                    tlastStart <- lastTStart `fmap` get
                    put $ s { lastTStop = lastTStart s + secs}
                    liftIO $ waitUntil t0 (tlastStart+secs)
                    return ())
                (do s <- get
                    put $ s { lastTStop = lastTStart s + secs} )

newtype Range a = Range {unRange :: Double -> a}

instance Functor Range where
    fmap f (Range r) = Range $ \x -> f $ r x

uniform lo hi = Range $ \x -> (hi-lo)*(realToFrac x)+lo
oneOf xs = Range $  \x -> let idx = round $ x*(realToFrac $ length xs -1)
                          in xs !! idx

always x = Range $ \_ -> x

uniformLog lo hi = let (llo, lhi) = (log lo, log hi)
                   in Range $ \x -> exp (unRange (uniform llo lhi) x)

pick :: MonadIO m => Range a -> m a
pick (Range f) = do
  unitVal <- liftIO $ randomRIO (0, 1)
  return $ f unitVal

--determine :: String -> [(String, Range V)] -> Goal
--determine = undefined

determine :: MonadIO m => CompiledToken -> [(String, Range V)] -> StateT QState m ()
determine tok rngs = do
  vals <- forM rngs $ \(nm, rng) -> liftM ((,) nm) (liftIO $ pick rng)
  invoke tok vals
  return ()
  

times :: Monad m => Int -> m () -> m ()
times 0 _ = return ()
times n m = m >> (n-1) `times` m
  

noDaqTrans ds = let daqVars = [ nm | ReadSource nm ("adc", _) <- ds ]
                    hasDaqVar (ReadSource nm _) = nm `elem` daqVars
                    hasDaqVar (SinkConnect (Var nm) _) = nm `elem` daqVars
                    hasDaqVar _ = False
                in filter (not . hasDaqVar) ds


