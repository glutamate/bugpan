module Compiler where

import Expr
import EvalM

data Stmt = InitSig String E
          | Env String E
          | SigUpdateRule String E
          | EventAddRule String E
          | SigSnkConn String String
          | SigSwitch String [(String, E)] E
            deriving (Eq, Show)

compile :: [Declare] -> [Stmt]
compile ds = let c = concatMap compileDec ds in
             concat [constEnv c, initSigVals c, mainLoop c] 

compileDec :: Declare -> [Stmt]
compileDec (Let nm (Sig se)) = [SigUpdateRule nm $ unVal se] 
compileDec (Let nm (SigDelay (Var sn) v0)) = [SigUpdateRule nm (Var sn), 
                                              InitSig nm v0]
compileDec (Let nm (Event ee)) = [EventAddRule nm $ unVal ee]
compileDec (Let nm (Switch ses ser)) = 
               [SigUpdateRule nm (Switch (map noSig ses) $ unSig ser)]
    where noSig (e,s) = (e, mapE unSig s)
          unSig (Sig se) = se
          unSig e = e


compileDec (Let nm e) = [Env nm e]
compileDec (SinkConnect (Var nm) snkNm) = [SigSnkConn nm snkNm]

unVal :: E -> E
unVal = mapE f
    where f (SigVal (Var n)) = Var n
          f e = e 

inMainLoop (SigUpdateRule _ _) = True
inMainLoop (EventAddRule _ _) = True
inMainLoop (SigSnkConn _ _) = True 
inMainLoop _ = False

ppStmt :: Stmt -> String
ppStmt (InitSig n e) = concat [n, "(0) = ", pp e]
ppStmt (Env n e) =  concat [n, " = ", pp e]
ppStmt (SigUpdateRule n e) =  concat [n, " = {: ", pp e, " :}"]
ppStmt (EventAddRule n e) =  concat [n, " = [: ", pp e, " :]"]
ppStmt (SigSnkConn vn sn) = concat [vn, " *> ", sn]

initSigVals stmts = [is | is@(InitSig nm v) <-  stmts]
constEnv stmts = [en | en@(Env nm v) <-  stmts]
mainLoop  stmts =  filter inMainLoop stmts

{- note: Now, 

Event :: [(T,a)] -> Event a

what if

Event :: ([(T,a)] -> [(T,a)]) -> Event a

or something like that
-}