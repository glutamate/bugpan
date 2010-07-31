{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Expr where

import Data.Generics
import Control.Monad

data U = D D | V V | E E | T T | S String

instance Show (V->Either String V) where
   show f = "<function>"

instance Read (V->Either String V) where
   readsPrec _ s = error $ "eq: <function>" ++s

instance Eq (V->Either String V) where
   f == g = error "eq: <function>"



data D = DLet [Pat] E
       | DMkType String [String] [(String, [T])]
       | DDecTy [String] T
       | DSource Pat String E
       | DSink E String E
       | DImport String
         deriving (Show, Eq, Read, Data, Typeable)

data V = VReal Double
       | VInt Int
       | VLam (V->Either String V)
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
       | ELet [(Pat,E)] E
       | ETy T E
         deriving (Show, Eq, Read, Data, Typeable)

data Pat = PLit V
         | PWild 
         | PVar String
         | PCons String [Pat]
         | PBang Pat
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


instance Monad (Either String) where
    return x = Right x
    (Left s) >>= _ = Left s
    (Right x) >>= f = f x
    fail = Left 

mapE :: (E -> E) -> E -> E
mapE f = everywhere (mkT f)

queryE :: (E-> [a]) -> E -> [a]
queryE qf = everything (++) ([] `mkQ` qf)

queryPat :: (Pat-> [a]) -> Pat -> [a]
queryPat qf = everything (++) ([] `mkQ` qf)

isSubTermIn :: E-> E-> Bool
isSubTermIn small big = not . null $ queryE tst big
    where tst someE | someE == small = [someE]
                    | otherwise = []

isFreeVarIn :: String-> E-> Bool
isFreeVarIn vnm = isSubTermIn (EVar vnm)

--substitute variables respecting shadowing
subVar n es (EVar n') | n == n' = es
                      | otherwise = EVar n'
subVar n es (EApp e1 e2) = EApp (subVar n es e1) (subVar n es e2)
subVar n es (ETy t e1) = ETy t (subVar n es e1)
subVar n es (ELam p e) | n `elem` patIntroducedVars p = ELam p e
                       | otherwise = ELam p (subVar n es e)
subVar n es (ECase e pates) = ECase e $ map (subVarPatE n es) pates
subVar n es (ELet pates e)  
    | n `elem` (concatMap patIntroducedVars $ map fst pates) = ELet (map (subVarPatE n es) pates) e
    | otherwise = ELet (map (subVarPatE n es) pates) $ subVar n es e
subVar n es (ECon v) = ECon v

subVarPatE n es (p,e) | n `elem` patIntroducedVars p = (p,e)
                      | otherwise = (p,subVar n es e)

patIntroducedVars :: Pat -> [String]
patIntroducedVars = queryPat f
    where f (PVar nm) = [nm]
          f p = []