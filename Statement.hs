module Statement where

import Expr
import EvalM
--import Data.HashTable as H
import Data.Map.Strict as H
import Control.Concurrent
import Numbers

data Stmt = InitSig String E
          | Env String E
          | SigUpdateRule String E
          | EventAddRule String E
          | SigSnkConn String String
          | RunPrepare (H.Map String V -> IO ())
          | RunInGLThread (H.Map String V -> IO ())
          | RunAfterDone (H.Map String V -> IO ())
          | RunAfterGo (H.Map String V -> IO ())
          | Trigger (H.Map String V -> IO ())
          | GLParams (MVar (IO V)) (MVar ())
          -- | SigSwitch String [(String, E)] E
          | ReadSrcAction String (RealNum -> RealNum -> IO V)
            deriving (Eq, Show)

instance Show (MVar a) where
    show mv = "<mvar>"
