module BuiltIn where

import Expr 
import Eval
import EvalM
--import Run
import Control.Monad
import Numbers
import Data.Char
import Debug.Trace
import Data.List
import HaskSyntaxUntyped
import UnitTesting hiding (evalsTo)

data BiV = BiV { bivName :: String,
                 bivType :: T,
                 bivVal :: V }

nstep t dt = roundNum (t/dt)

anyNumT = NumT Nothing
realT = NumT (Just RealT)
intT = NumT (Just IntT)
(.->.) = LamT
eventT t = PairT (anyNumT) (t)
eventsT t = ListT $ eventT t

bivs = [ 
 BiV "round" (realT .->. realT) (LamV $ \(NumV n)->return . NumV $ roundNum n),
 BiV "sin" (realT .->. realT) (LamV $ \(NumV n)->return . NumV $ sin n),
 BiV "cos" (realT .->. realT) (LamV $ \(NumV n)->return . NumV $ cos n),
 BiV "floor" (realT .->. realT) (LamV $ \(NumV n)->return . NumV $ floorNum n),
 BiV "enowAux" (anyNumT .->. (anyNumT .->. (eventsT AnyT .->. eventsT AnyT)))
         (LamV $ \(NumV t) -> return $ LamV $ \(NumV dt) -> return $ LamV $ \(ListV es) -> do
                                      let dropF (PairV (NumV te) _) = nstep te dt > nstep t dt
                                      let takeF (PairV (NumV te) _) = nstep te dt == nstep t dt
                                      return $ ListV (takeWhile takeF $ dropWhile dropF es)),
 BiV "sigTmax" (SignalT (TyVar "a") .->. realT) (LamV $ \s-> case s of 
                                                               (SigV t1 t2 dt df)-> return $ NumV (NReal t2)
                                                               _ -> error $ "buiiltin sigTmax: expected signal, got "++show s
                                                ),
 {-BiV "convAux" (SignalT realT .->. realT .->. eventsT AnyT .->. realT) (LamV $ \(SigV t1 t2 dt sf) -> return $ 
                                                                        LamV $ \(NumV (NReal tnow)) -> return $
                                                                        LamV $ \(ListV evvs) -> do
                                                                          return $ sum $ map (\(PairV tev n) -> n) evvs
                                                                       ),-}
 BiV "convAux" (SignalT realT .->. (realT .->. (eventsT AnyT .->. SignalT realT))) (LamV $ \x->return Unit),
 BiV "seconds" (SignalT realT) Unit,
 BiV "tmax" realT Unit,
 BiV "dt" realT Unit]

fstV (PairV x y) = x
sndV (PairV x y) = y

bivNms = [nm | BiV nm _ _ <- bivs ]

bivDecls = [Let (PatVar nm t) (Const vl) | BiV nm t vl <- bivs, not (nm `elem` ["seconds", "dt"]) ]
