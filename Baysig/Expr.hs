{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Expr where

import Data.Generics
import Control.Monad

data D = DLet Pat E
       | DType String [String] [(String, [T])]
       | DSource Pat String V
       | DSink E String V
         deriving (Show, Eq, Read, Data, Typeable)

data V = VReal Double
       | VInt Int
       | VBool Bool
       | VLam Pat E
       | VString String
       | VCons String [V]
         deriving (Show, Eq, Read, Data, Typeable)

data T = TLam T T
       | TApp T T
       | TCon String
       | TVar String
         deriving (Show, Eq, Read, Data, Typeable)

data E = ECon V
       | EApp E E
       | ELam Pat E
       | EVar String
       | ECase E [(Pat, E)]
       | EConstruct String [E]
       | ELet [(Pat,E)] E
         deriving (Show, Eq, Read, Data, Typeable)

data Pat = PLit V
         | PWild 
         | PVar String
         | PCons String [Pat]
           deriving (Show, Eq, Read, Data, Typeable)

e1 $> e2 = EApp e1 e2

instance Num E where
   e1 + e2 = EVar "+" $> e1 $> e2
   e1 - e2 = EVar "-" $> e1 $> e2
   e1 * e2 = EVar "*" $> e1 $> e2
   abs e = EVar "abs" $> e
   signum e = EVar "signum" $> e
   fromInteger n = ECon (VInt $ fromInteger n)

type Env = [(String, V)]

extEnv :: String -> V -> Env -> Env
extEnv nm v env = (nm,v):env

extsEnv :: [(String, V)] -> Env -> Env
extsEnv = (++) 

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
      VLam pat bd -> case match pat x of
                          Just exts -> eval (exts++env) bd 
                          Nothing -> fail $ "eval: incomplete pattern "++ show pat
      v -> fail $ "eval: expected lambda value, got: "++ show v
eval env (ELam pat bd) = return $ VLam pat bd
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
eval env (EConstruct nm es) = do
  vs <- mapM (eval env) es
  return $ VCons nm vs

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
match (PCons cnm1 pats) (VCons cnm2 vls) | cnm1 == cnm2 = matchCons $ zip pats vls
                                         | otherwise = Nothing
match (PCons _ _) v = Nothing

matchCons :: [(Pat, V)] -> Maybe [(String, V)]
matchCons [] = Just []
matchCons ((pat, v):patvs) = do
  env1 <- match pat v
  env <- matchCons patvs
  return $ env1++env
  
