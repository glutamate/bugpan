module Statement where

import Expr
import EvalM
import Data.HashTable as H
import Control.Concurrent
import Numbers

data Stmt = InitSig String E
          | Env String E
          | SigUpdateRule String E
          | EventAddRule String E
          | SigSnkConn String String
          | RunPrepare (H.HashTable String V -> IO ())
          | RunAfterDone (H.HashTable String V -> IO ())
          | RunAfterGo (H.HashTable String V -> IO ())
          | Trigger (H.HashTable String V -> IO ())
          | GLParams (MVar (IO V)) (MVar ())
          -- | SigSwitch String [(String, E)] E
          | ReadSrcAction String (RealNum -> RealNum -> IO V)
            deriving (Eq, Show)    

instance Show (MVar a) where
    show mv = "<mvar>"
 