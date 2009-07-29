module BuiltIn where

import Expr 
import Eval
import EvalM
--import Run
import Control.Monad
import Numbers
import Data.Char
--import Debug.Trace
import Data.List
import HaskSyntaxUntyped
import UnitTesting hiding (evalsTo)

data BiV = BiV { bivName :: String,
                 bivType :: T,
                 bivVal :: V }

nstep t dt = roundNum (t/dt)

anyNumT = NumT Nothing
(.->.) = LamT
eventT t = PairT (anyNumT) (t)
eventsT t = ListT $ eventT t

bivs = [ 
 BiV "round" (anyNumT .->. anyNumT) (LamV $ \(NumV n)->return . NumV $ roundNum n),
 BiV "sin" (anyNumT .->. anyNumT) (LamV $ \(NumV n)->return . NumV $ sin n),
 BiV "cos" (anyNumT .->. anyNumT) (LamV $ \(NumV n)->return . NumV $ cos n),
 BiV "floor" (anyNumT .->. anyNumT) (LamV $ \(NumV n)->return . NumV $ floorNum n),
 BiV "enowAux" (anyNumT .->. (anyNumT .->. (eventsT AnyT .->. eventsT AnyT)))
         (LamV $ \(NumV t) -> return $ LamV $ \(NumV dt) -> return $ LamV $ \(ListV es) -> do
                                      let dropF (PairV (NumV te) _) = nstep te dt > nstep t dt
                                      let takeF (PairV (NumV te) _) = nstep te dt == nstep t dt
                                      return $ ListV (takeWhile takeF $ dropWhile dropF es)),
 BiV "seconds" (SignalT anyNumT) Unit,
 BiV "tmax" anyNumT Unit,
 BiV "dt" anyNumT Unit]

bivNms = [nm | BiV nm _ _ <- bivs ]

bivDecls = [Let nm (Const vl) | BiV nm _ vl <- bivs, not (nm `elem` ["seconds", "dt"]) ]
