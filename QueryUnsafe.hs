module QueryUnsafe where

import Query
import System.IO.Unsafe
import Database
import Data.IORef
import QueryTypes
import Control.Monad.State.Lazy
import Data.List
import Expr
import EvalM
import Stages

{-# NOINLINE unsafeSession #-}
unsafeSession :: IORef (Maybe Session)
unsafeSession = unsafePerformIO . newIORef $ Nothing

openLastSession :: IO ()
openLastSession = do sess <- lastSession "/home/tomn/sessions"
                     writeIORef unsafeSession $ Just sess

openNewSession :: IO ()
openNewSession = do sess <- newSession "/home/tomn/sessions"
                    writeIORef unsafeSession $ Just sess

deleteCurrentSession = do Just sess <- readIORef unsafeSession
                          deleteSession sess


usignals :: Reify a => String -> a-> [Signal a]
usignals nm proxy = unsafePerformIO $ do sess <- readIORef unsafeSession
                                         case sess of 
                                           Just s -> evalStateT (signals nm proxy) s
                                           Nothing -> error $ "usingals "++nm++": no open session"

uevents :: Reify a => String -> a-> [Event a]
uevents nm proxy = unsafePerformIO $ do sess <- readIORef unsafeSession
                                        case sess of 
                                           Just s -> evalStateT (events nm proxy) s
                                           Nothing -> error $ "uevents "++nm++": no open session"

udurations :: Reify a => String -> a-> [Duration a]
udurations nm proxy = unsafePerformIO $ do sess <- readIORef unsafeSession
                                           case sess of 
                                             Just s -> evalStateT (durations nm proxy) s
                                             Nothing -> error $ "udurations "++nm++": no open session"


urun :: [Declare] -> Double -> IO ()
urun ds t0 = do
  Just sess <- readIORef unsafeSession
  let trun = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  --liftIO $ mapM (putStrLn . ppDecl) ds
  liftIO $ runOnce dt t0 trun ds sess
