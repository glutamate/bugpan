{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSignatures, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-} 

module QueryTypes where

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
import System.Random
import System.Cmd
--import System.Info.MAC as MAC
--import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L
--import Data.ByteString.Internal
import qualified Data.Binary as B
import Numeric
import Traverse
import Transform
--import Stages
import Data.Ord
--import Charts
import Control.Concurrent
import Database
import HaskSyntaxUntyped

type Duration = (Double,Double,V)
type Event = (Double,V)

type Signal = V

type List a = [a]
type Id a = a

vToEvent v = (evTime v, evTag v)
vToDuration v = let (t1, t2) = epTs v in (t1, t2, epTag v)

--class (MonadState Session m, MonadIO m) => QueryM m where
--    answers :: [a] -> m a

--class MCompose m1 m2 m3 | m1 m2 -> m3 where
--    mcompose :: 
    


--instance QueryM (ListT (StateT Session IO)) Id where
 --   answers xs = ListT . return $ xs

--instance QueryM (StateT Session IO) where
--    answers = return 


--newtype AskM a = AskM { unAskM :: ListT (StateT Session IO) a }
 --   deriving (Monad, MonadIO, Functor, MonadState Session, MonadPlus)

--runAskM :: Session -> AskM a -> IO [a]
--runAskM sess (AskM lsioA) = fst `fmap` runStateT (runListT (lsioA)) sess

--answers :: [a] -> AskM a
--answers xs = AskM (ListT . return $ xs)
--answer x = AskM (ListT . return $ [x])


--old stuff 
{-askM :: Q -> AskM V
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


data Q = QVar String
       -- | Filter E Q
       -- | Map E Q
       | Filter E Q
       | Map E Q
       | Has Q Q
       | In Q Q
       | Around Q Q
-}