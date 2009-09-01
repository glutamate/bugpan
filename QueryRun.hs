{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts#-} 

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

--type QState = (Session)

compile :: [Declare] -> StateT QState IO (String)
compile ds = do
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  let sha = take 50 . showDigest . sha512 . L.pack $ map c2w $ show ds
  let dsTrans = snd $ runTravM ds [] transform
  --liftIO $ mapM_ (putStrLn . ppDecl) dsTrans
  liftIO $ whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
                 (do setCurrentDirectory "/var/bugpan/queryCache/"
                     compileToHask (sha++".hs") dt trun dsTrans
                     system $ "ghc -O2 --make "++sha
                     return ())
  liftIO $ whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
             (fail "could not compile")
  --hash declares, look in cache
  return sha

invoke :: String -> StateT QState IO ()
invoke sha = do
  Session sessNm _ <- getSession
  liftIO $ system $ "/var/bugpan/queryCache/"++sha++" "++(last $ splitBy '/' sessNm)
  return ()

run :: [Declare] -> RealNum -> StateT QState IO ()
run ds t0 = do
  sess <- getSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess

use :: MonadIO m => String -> m [Declare]
use fnm = liftIO $ fileDecls fnm []

with = flip makeSubs
