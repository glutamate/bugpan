{-# LANGUAGE FlexibleInstances, OverloadedStrings #-} -- , MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction  give up on types -}

module HaskSyntaxUntyped where

import Expr 
import qualified Expr as E
import EvalM
import Data.List
import Numbers
import Data.String

default (Int, Double)

infixl 1 =:

class Assignable s where
    (=:) :: String -> E -> s

instance Assignable Declare where
    x =: y = let (nm:vars) = splitBySpaces x in 
             Let nm $ addLams (reverse vars) y 

instance Assignable (String, E) where -- LetE
    x =: y = (x, y)

instance Assignable (E, E) where -- LetE
    x =: y = (Var x, y)



addLams [] e = e
addLams (v:vs) e = addLams vs $ Lam v e


splitBySpaces :: String -> [String]
splitBySpaces = filter (not. null) . splitBySpaces'

splitBySpaces' [] = []
splitBySpaces' (' ':s) = splitBySpaces s
splitBySpaces' s = let (hd, tl) = span (/=' ') s 
                   in hd : splitBySpaces tl

instance IsString E where
    fromString s = Var s


infixl 1 ~>                    

x ~> y = (x, y)            

true, false :: E 
true = Const . BoolV $ True
false = Const . BoolV $ False

lam :: String -> E -> E
lam vn bod = let vars = splitBySpaces vn in 
             addLams (reverse vars) bod
             
sig = Sig
delay = SigDelay
val = SigVal


list :: [E] -> E
list [] = Nil
list (x:xs) = Cons x (list xs)

pair x y = Pair x y

foo = If true "x" "y"

bar :: E
bar = (lam "x y z" "x") $> 1

baz :: Declare
baz = "f x" =: "x"

infixl 1 *>
infixl 1 <*                   

x *> y = SinkConnect x y            
x <* y = ReadSource x $ splitBySpaces y            
