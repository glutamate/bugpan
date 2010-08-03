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

fromRight (Right x) = x
fromRights (Right xs) = xs
fromRights _ = []

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
    return $ VLam $ \x -> do exts <- matchPV pat x 
                             eval (extsEnv exts env) bd 
--                             Nothing -> fail $ "eval: incomplete pattern "++ show pat
{-eval env (ELet pates e) = vfinal
    where f (p,ex) = do 
                 env' <- envExtsM
                 v <- eval env' ex
                 matchPV p v
          envExtsM = do nvals <- concat `fmap` sequence (return (vals env): map f pates)
                        return $ env {vals = nvals}
          vfinal = do 
                 env' <- envExtsM
                 eval env' e -}
eval env (ELet pates e) = vfinal
    where f (p,ex) = fromRights $ eval envExtsM ex >>= matchPV p
          envExtsM = let nvals = concat $ (vals env) : map f pates
                     in env {vals = nvals}
          vfinal =eval envExtsM e 
          
{-eval env (ELet [] bd) = eval env bd
eval env (ELet ((pat,e):rest) bd) = do
  v <- eval env e
  exts <- matchPV pat v 
  eval (extsEnv exts env) (ELet rest bd) -}

eval env (ECase ex pats) = do
  v <- eval env ex
  evalCase env v pats

evalCase :: Env -> V -> [(Pat, E)] -> Either String V
evalCase env v [] = Left $ "evalCase: non-exhaustive case; no match for: "++show v
evalCase env v ((pat,e):rest) = 
    case matchPV pat v of
      Just exts -> eval (extsEnv exts env) e
      Nothing -> evalCase env v rest

matchE :: MonadPlus m => Pat -> E -> m [(String, E)]
matchE p (ECon v) = do nmvs <- matchPV p v
                       forM nmvs $ \(nm,v) -> return (nm, ECon v)
matchE (PCons nm []) (EVar nm') | nm == nm' = return []
                                | otherwise = mzero
matchE (PCons nm []) e@(EApp f arg) = fail $ "matchE no match "++show e
matchE (PCons nm ps) (EApp f arg) = do 
       this_exts<- matchE (last ps) arg
       more_exts<- matchE (PCons nm (init ps)) f
       return $ this_exts ++ more_exts
matchE (PVar nm) e = return [(nm, e)]
matchE _ _ = fail "matchE fail"

matchPV :: MonadPlus m => Pat -> V -> m [(String, V)]
matchPV (PVar nm) v = return [(nm, v)]
matchPV PWild v = return []
matchPV (PBang p) v = matchPV p v
matchPV (PTy t p) v = matchPV p v
matchPV (PLit v1) v2 | v1 == v2 = return []
                     | otherwise = mzero
matchPV (PCons cnm1 pats) (VCons cnm2 vls) | cnm1 == cnm2 = matchCons $ zip pats vls
                                         | otherwise = mzero
matchPV (PCons _ _) v = mzero

matchCons :: MonadPlus m => [(Pat, V)] -> m [(String, V)]
matchCons [] = return []
matchCons ((pat, v):patvs) = do
  env1 <- matchPV pat v
  env <- matchCons patvs
  return $ env1++env
  
 