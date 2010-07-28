{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Expr where

import Data.Generics
import Control.Monad

data D = DLet [Pat] E
       | DMkType String [String] [(String, [T])]
       | DDecTy [String] T
       | DSource Pat String E
       | DSink E String E
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
