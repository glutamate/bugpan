{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Eval where

import Baysig.Expr
import Control.Monad

data Env = Env { vals :: [(String, V)],
                 constrs :: [(String, [T])] }
           deriving Show

emptyEnv = Env [] []

extEnv :: (String, V) -> Env -> Env
extEnv nmv env = env { vals = nmv : vals env }

extsEnv :: [(String, V)] -> Env -> Env
extsEnv svs env =  foldl (flip extEnv) env svs

lookupM nm assocs = 
    case lookup nm assocs of
      Nothing -> fail $ "eval: cannot find variable "++nm++" in evironment"
      Just v -> return v

wrapConstr :: String -> [T] -> Either String V
wrapConstr nm ts = wC nm ts []
    where wC nm [] vls = return $ VCons nm vls
          wC nm (t:ts) vls = return $ VLam $ \v -> wC nm ts (v:vls)    

instance MonadPlus (Either String) where
    mzero = Left "mzero"
    mplus (Left _) x = x
    mplus rx _ = rx

instance Functor (Either String) where
    fmap f (Left x) = Left x
    fmap f (Right x) = Right $ f x

eval :: Env -> E -> Either String V
eval _   (ECon v) = return v
eval env (ETy _ e ) = eval env e
eval env (EVar nm) = (do 
    ts <- lookupM nm (constrs env)
    wrapConstr nm ts
    ) `mplus` 
    lookupM nm (vals env)
    
eval env (EApp ef ex) = do
    x <- eval env ex
    f <- eval env ef
    case f of
      VLam vf -> vf x
      v -> fail $ "eval: expected lambda value, got: "++ show v
eval env (ELam pat bd) = 
    return $ VLam $ \x -> do exts <- match pat x 
                             eval (extsEnv exts env) bd 
--                             Nothing -> fail $ "eval: incomplete pattern "++ show pat
{-eval env (ELet pates e) = vfinal
    where f (p,ex) = do 
                 env' <- envExtsM
                 v <- eval env' ex
                 case match p v of
                   Just exts -> return exts
                   Nothing -> fail $ "eval: incomplete pattern "++ 
                                     show p ++"for value "++show v    -- [m [(S,V)]]
          envExtsM = do nvals <- concat `fmap` sequence (return (vals env): map f pates)
                        return $ env {vals = nvals}
          vfinal = do 
                 env' <- envExtsM
                 eval env' e -}
          
eval env (ELet [] bd) = eval env bd
eval env (ELet ((pat,e):rest) bd) = do
  v <- eval env e
  exts <- match pat v 
  eval (extsEnv exts env) (ELet rest bd) 

eval env (ECase ex pats) = do
  v <- eval env ex
  evalCase env v pats

evalCase :: Env -> V -> [(Pat, E)] -> Either String V
evalCase env v [] = Left $ "evalCase: non-exhaustive case; no match for: "++show v
evalCase env v ((pat,e):rest) = 
    case match pat v of
      Just exts -> eval (extsEnv exts env) e
      Nothing -> evalCase env v rest

match :: MonadPlus m => Pat -> V -> m [(String, V)]
match (PVar nm) v = return [(nm, v)]
match PWild v = return []
match (PBang p) v = match p v
match (PLit v1) v2 | v1 == v2 = return []
                   | otherwise = mzero
match (PCons cnm1 pats) (VCons cnm2 vls) | cnm1 == cnm2 = matchCons $ zip pats vls
                                         | otherwise = mzero
match (PCons _ _) v = mzero

matchCons :: MonadPlus m => [(Pat, V)] -> m [(String, V)]
matchCons [] = return []
matchCons ((pat, v):patvs) = do
  env1 <- match pat v
  env <- matchCons patvs
  return $ env1++env
  
 