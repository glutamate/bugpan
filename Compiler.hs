module ImpInterpret where

import Expr
import EvalM

data Stmt = InitSig String E
          | Env String E
          | SigUpdateRule String E
          | EventAddRule String E

compileDec :: Declare -> [Stmt]
compileDec (Let nm (Sig se)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let nm (SigDelay (Var sn) v0)) = [SigUpdateRule nm (Var sn), 
                                              InitSig nm v0]
compileDec (Let nm (Event ee)) = [EventAddRule nm ee] 
compileDec (Let nm e) = [Env nm e]

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

isSigOrEvtS (SigUpdateRule _ _) = True
isSigOrEvtS (EventAddRule _ _) = True
isSigOrEvtS _ = False
