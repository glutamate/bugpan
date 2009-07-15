{-# LANGUAGE FlexibleInstances, OverloadedStrings #-} -- , MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction  give up on types -}

module HaskSyntaxUntyped where

import Expr 
import qualified Expr as E
import EvalM
import Data.List
import Numbers
import Data.String
import Data.Unique
import System.IO.Unsafe

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


unsafeUniqueInt = unsafePerformIO (hashUnique `fmap` newUnique)


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
         
lam' :: (E->E) -> E    
lam' bodf = bodf $ (Var ("unique_var_"++show unsafeUniqueInt))

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

dbl :: Double -> E
dbl = Const . NumV . NReal

int :: Int -> E
int = Const . NumV . NInt

cdbl :: Double -> V
cdbl =  NumV . NReal

cint :: Int -> V
cint = NumV . NInt

class ToVal a where
	toVal :: a-> V

instance ToVal Int where
	toVal = NumV . NInt

instance ToVal Double where
	toVal = NumV . NReal

instance ToVal [Char] where
	toVal = StringV

instance ToVal a => ToVal [a] where
	toVal xs = ListV $ map toVal xs

instance ToVal () where
	toVal _ = Unit

instance (ToVal a, ToVal b) => ToVal (a,b) where
	toVal (x,y) = PairV (toVal x) (toVal y)





