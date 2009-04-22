module ImpInterpret where

import Expr
import EvalM
import Compiler

exec :: [Stmt] -> [(String, [V])]
exec stmts = 
    let initsigs = [(nm,is) | InitSig nm v <- stmts]
        prg =  filter isSigOrEvtS stmts
               

