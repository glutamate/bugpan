{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, ExistentialQuantification #-} 

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


type CompiledToken = (String,Double,[(String, T)] )

compile :: [Declare] -> [(String, T)] -> StateT QState IO CompiledToken
compile ds params = do
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  let commentParams = map (\(nm,ty)-> Comment $ "PARAMETER: "++nm++ppType ty) params
  let sha = take 50 . showDigest . sha512 . L.pack $ map c2w $ show (ds++commentParams)
  let dsTrans = snd $ runTravM ds [] transform
  --liftIO $ mapM_ (putStrLn . ppDecl) dsTrans
  liftIO $ createDirectoryIfMissing False "/var/bugpan/queryCache/"
  liftIO $ whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
                 (do setCurrentDirectory "/var/bugpan/queryCache/"
                     compileToHask (sha++".hs") dt trun dsTrans params
                     system $ "ghc -O2 --make "++sha -- -prof -auto-all
                     return ())
  liftIO $ whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
             (fail "could not compile")
  --hash declares, look in cache
  return (sha, trun, params)

invoke :: CompiledToken ->[(String,V)] -> StateT QState IO ()
invoke (sha, tmax,pars) vals= do
  s <- get
  t0 <- getTnow
  Session sessNm _ <- getSession
  let valargs = intercalate " " $ map (ppVal . snd) vals --ideally check ordering
  let cmdStr = "time /var/bugpan/queryCache/"++sha++" "++(last $ splitBy '/' sessNm)++" "++show t0 ++" "++valargs
  liftIO . putStrLn $ cmdStr
  liftIO $ putStrLn ""
  liftIO $ system $ cmdStr -- ++" +RTS -p"
  put $ s { lastTStart = t0,
            lastTStop = t0 + tmax}
  return ()

run :: [Declare] -> StateT QState IO ()
run ds = do
  s <- get
  sess <- getSession
  t0 <- getTnow
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess
  put $ s { lastTStart = t0,
            lastTStop = t0 + trun}

getTnow = ifM (realTime `fmap` get)
              (do Session _ t0 <- getSession
                  tnow <- liftIO $ getClockTime
                  return $ diffInS tnow t0)
              (lastTStop `fmap` get)

use :: MonadIO m => String -> m [Declare]
use fnm = liftIO $ fileDecls fnm []

with = flip makeSubs

pause :: Double -> StateT QState IO ()
pause secs= ifM (realTime `fmap` get)
                (do s <- get
                    Session _ t0 <- getSession
                    tlastStart <- lastTStart `fmap` get
                    put $ s { lastTStop = lastTStart s + secs}
                    liftIO $ waitUntil t0 (tlastStart+secs)
                    return ())
                (do s <- get
                    put $ s { lastTStop = lastTStart s + secs} )


data Goal = Run String [(String,V)]
          | Wait Double
          | forall a. IfG [a] Goal Goal
          | Goal :>>: Goal

type Range a = Double -> a

uniform lo hi = \x -> (hi-lo)*(realToFrac x)+lo
oneOf xs = \x -> let idx = round $ x*(realToFrac $ length xs -1)
                 in xs !! idx

always x = \_ -> x

uniformLog lo hi = let (llo, lhi) = (log lo, log hi)
                   in \x -> exp (uniform llo lhi x)

pickFromRange :: Range a -> IO a
pickFromRange f = do
  unitVal <- randomRIO (0, 1)
  return $ f unitVal

--determine :: String -> [(String, Range V)] -> Goal
--determine = undefined

determine :: CompiledToken -> [(String, Range V)] -> StateT QState IO ()
determine tok@(sha, tmax,parlist) rngs = do
  vals <- forM rngs $ \(nm, rng) -> liftM ((,) nm) (liftIO $ pickFromRange rng)
  invoke tok vals
  return ()
  

times :: Monad m => Int -> m () -> m ()
times 0 _ = return ()
times n m = m >> (n-1) `times` m
  

optimise :: String -> [(String, Range V)] -> ([(String, V)] -> Double) -> Goal
optimise = undefined

until :: [a] -> Goal -> Goal
until = undefined
