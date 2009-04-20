module ImpInterpret where

import Expr
import EvalM

data Stmt = InitSig String V
          | Env String V
          | UpdateRule String E

compileDec :: Declare -> [Stmt]
compileDec (Let nm (Sig se)) = [UpdateRule nm $ unVal se] 
compileDec (Let nm (SigDelay (Var sn) v0)) = [UpdateRule nm (Var sn), 
                                              InitSig nm $ eval v0]
compileDec (Let nm e) = [Env nm $ eval e]

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

eval :: E -> V
eval = undefined

exec :: [Stmt] -> [(String, [V])]
exec = undefined