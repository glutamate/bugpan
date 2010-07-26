{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Bugpan.Expr where

import Data.Generics
import Control.Monad

data V = VReal Double
       | VInt Int
       | VBool Bool
       | VLam String E
       | VPair V V
       | VUnit
       | VList [V]
       | VString String
         deriving (Show, Eq, Read, Data, Typeable)

data T = TInt
       | TReal
       | TLam T T
       | TBool
       | TyApp T T
       | TyCon String
         deriving (Show, Eq, Read, Data, Typeable)

data E = ECon V
       | EApp E E
       | ELam String T E
       | EVar String
       | ECase E [(Pat, E)]
       | ELet [(Pat,E)] E
         deriving (Show, Eq, Read, Data, Typeable)

data Pat = PLit V
         | PWild 
         | PVar String
         | PPair Pat Pat
           deriving (Show, Eq, Read, Data, Typeable)

type Env = [(String, V)]

extEnv :: String -> V -> Env -> Env
extEnv nm v env = (nm,v):env

extsEnv :: [(String, V)] -> Env -> Env
extsEnv [] env = env
extsEnv ((nm,v):exts) env = extsEnv exts $ (nm,v):env

instance Monad (Either String) where
    return x = Right x
    (Left s) >>= _ = Left s
    (Right x) >>= f = f x
    fail = Left 

eval :: Env -> E -> Either String V
eval _   (ECon v) = return v
eval env (EVar nm) = 
    case lookup nm env of
      Nothing -> fail $ "eval: cannot find variable "++nm++" in evironment"
      Just v -> return v
eval env (EApp ef ex) = do
    x <- eval env ex
    f <- eval env ef
    case f of
      VLam nm bd -> eval (extEnv nm x env) bd 
      v -> fail $ "eval: expected lambda value, got: "++ show v
eval env (ELam nm _ bd) = return $ VLam nm bd
eval env (ELet [] bd) = eval env bd
eval env (ELet ((pat,e):rest) bd) = do
  v <- eval env e
  let exts = case match pat v of
               Nothing -> []
               Just es -> es
  let nEnv = extsEnv (exts) env
  eval (nEnv) (ELet rest bd)

eval env (ECase ex pats) = do
  v <- eval env ex
  evalCase env v pats 

evalCase :: Env -> V -> [(Pat, E)] -> Either String V
evalCase env v [] = Left $ "evalCase: non-exhaustive case; no match for: "++show v
evalCase env v ((pat,e):rest) = 
    case match pat v of
      Just exts -> eval (extsEnv exts env) e
      Nothing -> evalCase env v rest

match ::  Pat -> V -> Maybe [(String, V)]
match (PVar nm) v = Just [(nm, v)]
match PWild v = Just []
match (PLit v1) v2 | v1 == v2 = Just []
                   | otherwise = Nothing
match (PPair px py) (VPair vx vy) = do
  boundx <- match px vx
  boundy <- match py vy
  return $ boundx++boundy
match (PPair px py) _ = Nothing

