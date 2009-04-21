module ImpInterpret where

import Expr
import EvalM

data Stmt = InitSig String V
          | Env String V
          | SigUpdateRule String E
          | EventAddRule String E

compileDec :: Declare -> [Stmt]
compileDec (Let nm (Sig se)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let nm (SigDelay (Var sn) v0)) = [SigUpdateRule nm (Var sn), 
                                              InitSig nm $ eval v0]
compileDec (Let nm (Event ee)) = [EventAddRule nm ee] 
compileDec (Let nm e) = [Env nm $ eval e]

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

eval :: E -> V
eval = undefined

isSigOrEvt (SigUpdateRule _ _) = True
isSigOrEvt (EventAddRule _ _) = True
isSigOrEvt _ = False

exec :: [Stmt] -> [(String, [V])]
exec stmts = 
    let initsigs = [(nm,is) | InitSig nm v <- stmts]
        prg =  filter isSigOrEvt stmts
               

