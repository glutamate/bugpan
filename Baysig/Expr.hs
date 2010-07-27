{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Expr where

import Data.Generics
import Control.Monad

data D = DLet [Pat] E
       | DMkType String [String] [(String, [T])]
       | DDecTy [String] T
       | DSource Pat String V
       | DSink E String V
       | DImport String
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
       | ETy T E
         deriving (Show, Eq, Read, Data, Typeable)

data Pat = PLit V
         | PWild 
         | PVar String
         | PCons String [Pat]
           deriving (Show, Eq, Read, Data, Typeable)

e1 $> e2 = EApp e1 e2

lift1 s e = EVar s $> e

instance Num E where
   e1 + e2 = EVar "+" $> e1 $> e2
   e1 - e2 = EVar "-" $> e1 $> e2
   e1 * e2 = EVar "*" $> e1 $> e2
   abs e = EVar "abs" $> e
   signum e = EVar "signum" $> e
   fromInteger n = ECon (VInt $ fromInteger n)

instance Fractional E where
    fromRational rat = ECon (VReal $ fromRational rat)

instance Floating E where
    pi = ECon (VReal pi)
    exp e = lift1 "exp" e
    log e = lift1 "log" e
    sin e = lift1 "sin" e 
    cos e = lift1 "cos" e
    tan e = lift1 "tan" e
    acos e = lift1 "acos" e
    asin e = lift1 "asin" e
    atan e = lift1 "atan" e
    sinh e = lift1 "sinh" e 
    cosh e = lift1 "cosh" e
    asinh e = lift1 "asinh" e 
    acosh e = lift1 "acosh" e
    atanh e = lift1 "atanh" e

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
eval env (ETy _ e ) = eval env e
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

match :: Pat -> V -> Maybe [(String, V)]
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
  
