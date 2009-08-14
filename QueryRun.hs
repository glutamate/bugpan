{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts#-} 

module QueryRun where

import EvalM
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
import qualified Control.Monad.List as L
import Control.Monad.State.Lazy
import System.Directory
import System.Time
import System.Random

--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L
--import Data.ByteString.Internal
import qualified Data.Binary as B
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

--type QState = (Session)


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


{-plot :: [V] -> IO ()
plot vs = do --let g = map ansToPlot ans
             plotGraph (valsToGraph vs)
             return ()
          

valsToGraph :: [V] -> Graph
valsToGraph vs = foldl1 (<+>) $ map vToPlot vs
    where vToPlot (SigV t1 t2 dt sf)= toGraph ((toPlot $map (\t -> (t, unsafeVToDbl $ sf t)) [t1, t1+dt..t2])%Lines)
          vToPlot e  | isEvent e = toGraph ((toPlot [(evTime e, unsafeVToDbl $ evTag e)])%FilledCircles)
          vToPlot ep | isEpoch ep = 
                         let (t1,t2) = epTs ep 
                             epvl = unsafeVToDbl $ epTag ep in
                         toGraph ((toPlot [(t1, epvl), (t2, epvl)])%Lines) 

-}

{-testQ = do s <- lastSession "/home/tomn/sessions/"
           print s
           qres <- runAskM s $ signals "vm"
           mapM plotWithR qres
                         
           --mapM print qres
           return () -}