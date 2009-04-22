module Compiler where

import Expr
import EvalM

data Stmt = InitSig String E
          | Env String E
          | SigUpdateRule String E
          | EventAddRule String E
            deriving (Eq, Show)

compile :: [Declare] -> [Stmt]
compile ds = let c = concatMap compileDec ds in
             concat [constEnv c, initSigVals c, mainLoop c] 

compileDec :: Declare -> [Stmt]
compileDec (Let nm (Sig se)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let nm (SigDelay (Var sn) v0)) = [SigUpdateRule nm (Var sn), 
                                              InitSig nm v0]
compileDec (Let nm (Event ee)) = [EventAddRule nm $ unVal ee] 
compileDec (Let nm e) = [Env nm e]

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

isSigOrEvtS (SigUpdateRule _ _) = True
isSigOrEvtS (EventAddRule _ _) = True
isSigOrEvtS _ = False

ppStmt :: Stmt -> String
ppStmt (InitSig n e) = concat [n, "(0) = ", pp e]
ppStmt (Env n e) =  concat [n, " = ", pp e]
ppStmt (SigUpdateRule n e) =  concat [n, " = {: ", pp e, " :}"]
ppStmt (EventAddRule n e) =  concat [n, " = [: ", pp e, " :]"]

initSigVals stmts = [is | is@(InitSig nm v) <-  stmts]
constEnv stmts = [en | en@(Env nm v) <-  stmts]
mainLoop  stmts =  filter isSigOrEvtS stmts