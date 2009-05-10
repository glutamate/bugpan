{-# LANGUAGE FlexibleInstances, UndecidableInstances #-} -- , MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction  give up on types -}

module HaskSyntax where

import Expr hiding (($>))
import qualified Expr as E
import EvalM
import Data.List
import Numbers

default (Int, Double)

infixl 1 =:

class Assignable s where
    (=:) :: String -> E -> s

instance Assignable Declare where
    x =: y = let (nm:vars) = splitBySpaces x in 
             Let nm $ addLams (reverse vars) y 

instance Assignable (String, E) where -- LetE
    x =: y = (x, y)

addLams [] e = e
addLams (v:vs) e = addLams vs $ Lam v e


splitBySpaces :: String -> [String]
splitBySpaces = filter (not. null) . splitBySpaces'

splitBySpaces' [] = []
splitBySpaces' (' ':s) = splitBySpaces s
splitBySpaces' s = let (hd, tl) = partition (/=' ') s 
                   in hd : splitBySpaces tl


class ToE a where
    toExpr :: a -> E

instance ToE E where
    toExpr = id

instance ToE [Char] where
    toExpr = Var

instance ToE Int where
    toExpr i = Const . NumV . NInt $ i

instance ToE Double where
    toExpr i = Const . NumV . NReal $ i


sig :: ToE a => a -> E
sig = Sig . toExpr

sigVal :: ToE a => a -> E
sigVal = SigVal . toExpr

sigDelay :: (ToE a, ToE b) => a -> b-> E
sigDelay x y = SigDelay (toExpr x) (toExpr y)

x $> y = (toExpr x) E.$> (toExpr y)



infixl 1 ~>                    

x ~> y = (x, y)            


true, false :: E 
true = Const . BoolV $ True
false = Const . BoolV $ False

iff ::  (ToE p, ToE c, ToE a) =>p -> c -> a -> E
iff p c a = If (toExpr p) (toExpr c)  (toExpr a) 

lam :: ToE b => String -> b -> E
lam vn bod = --let vars = splitBySpaces vn in  .. addLams (reverse vars) 
             Lam vn (toExpr bod)

foo = iff true "x" "y"

bar :: E
bar = (lam "x" "x") $> 1