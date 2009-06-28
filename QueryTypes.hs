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
import Data.Maybe

type Duration a = (Double,Double,a)
type Event a = (Double,a)

data Signal a = Signal Double Double Double (Int -> a) 

type List a = [a]
type Id a = a

vToEvent v = (evTime v, evTag v)
vToDuration v = let (t1, t2) = epTs v in (t1, t2, epTag v)


showDur (t1,t2,v) = show t1 ++ ".."++show t2++": "++show v
showEvt (t,v) = "@"++show t++": "++show v


class Tagged t where
    getTag :: t a-> a
    setTag :: t a-> a ->t a

instance Tagged ((,) Double) where
    getTag = snd
    setTag (t,_) v = (t,v)

instance Tagged ((,,) Double Double) where
    getTag (_,_,v) = v
    setTag (t1,t2,_) v = (t1,t2, v)

instance Functor ((,) Double) where
    fmap f (t,v) = (t, f v)

instance Functor ((,,) Double Double) where
    fmap f (t1,t2, v) = (t1,t2,f v)
 
instance Functor Signal where
    fmap f (Signal t1 t2 dt sf) = 
        Signal t1 t2 dt $ \ix -> f (sf ix)

class Reify a where
    reify :: V-> Maybe a
    pack :: a->V

instance Reify V where 
    reify = Just
    pack = id
instance Reify Double where 
    reify = vToDbl
    pack = NumV . NReal
instance Reify Int where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just i
    reify _ = Nothing
    pack = NumV . NInt
instance Reify Integer where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just $ toInteger i
    reify _ = Nothing
    pack = NumV . NInt . fromInteger
instance (Reify a, Reify b) => Reify (a,b) where
    reify (PairV a b) = liftM2 (,) (reify a) (reify b)
    reify _ = Nothing
    pack (x,y) = PairV (pack x) (pack y)
instance (Reify a, Reify b, Reify c) => Reify (a,b,c) where
    reify (PairV (PairV a b) c) = liftM3 (,,) (reify a) (reify b) (reify c)
    reify _ = Nothing
    pack (x,y,z) = PairV (PairV (pack x) (pack y)) (pack z)
instance (Reify a) => Reify [a] where
    reify (ListV xs) = let rmxs = map reify xs in
                       if all isJust rmxs
                          then Just $ map fromJust rmxs
                          else Nothing
    reify _ = Nothing
    pack xs = ListV $ map pack xs
instance Reify NumVl where
    reify (NumV n) = Just n
    reify _ = Nothing
    pack = NumV
instance Reify () where
    reify Unit = Just ()
    reify _ = Nothing
    pack () = Unit
instance Reify a => Reify (Signal a) where
    reify (SigV t1 t2 dt sf) = Just $ Signal t1 t2 dt $ \ix-> unsafeReify (sf ix)
    pack (Signal t1 t2 dt sf) = SigV t1 t2 dt $ \ix->pack (sf ix)

unsafeReify :: Reify a => V -> a
unsafeReify = fromJust . reify

--instance VFunctor Duration where

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