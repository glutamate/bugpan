{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSignatures #-} 

module Query where

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
import Numeric
import Traverse
import Transform
import Stages
import Data.Ord
import Charts
import Control.Concurrent
import Database


evInEpoch ev ep = let (t1, t2) = epTs ep 
                      tev = evTime ev
                  in tev<t2 && tev>t1

getSortedDirContents dir = do conts <- getDirContents dir
                              let sconts = sortBy cmpf conts
                              liftIO $ print sconts
                              return sconts
    where cmpf f1 f2 = case (readsPrec 5 f1, readsPrec 5 f2) of
                         ((n1::Int,_):_, (n2::Int,_):_) -> compare n1 n2
                         _ -> EQ



newtype AskM a = AskM { unAskM :: ListT (StateT Session IO) a }
    deriving (Monad, MonadIO, Functor, MonadState Session, MonadPlus)

runAskM :: Session -> AskM a -> IO [a]
runAskM sess (AskM lsioA) = fst `fmap` runStateT (runListT (lsioA)) sess

answers :: [a] -> AskM a
answers xs = AskM (ListT . return $ xs)
answer x = AskM (ListT . return $ [x])

signals :: String -> AskM V
signals nm = do
  Session bdir t0 <- get
  --liftIO . print $ bdir++"/signals/"++nm
  ifM (liftIO (doesDirectoryExist (bdir++"/signals/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/signals/"++nm
          sigs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/signals/"++nm++"/"++fn
          answers sigs)
      (answers [])

events :: String -> AskM V
events nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/events/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/events/"++nm
          evs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/events/"++nm++"/"++fn
          answers (concat evs))
      (answers [])

epochs :: String -> AskM V
epochs nm = do
  Session bdir t0 <- get
  ifM (liftIO (doesDirectoryExist (bdir++"/epochs/"++nm)))
      (do fnms <- getSortedDirContents $ bdir++"/epochs/"++nm
          evs <- forM fnms $ \fn-> liftIO $ loadBinary $ bdir++"/epochs/"++nm++"/"++fn
          answers evs)
      (answers [])

askM :: Q -> AskM V
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
            

plot :: [V] -> IO ()
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


data Q = QVar String
       -- | Filter E Q
       -- | Map E Q
       | Filter E Q
       | Map E Q
       | Has Q Q
       | In Q Q
       | Around Q Q

