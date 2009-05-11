{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module HaskSyntax where

import qualified Expr as U
import EvalM
import Data.List
import Numbers
import Data.String

default (Int, Double)


newtype TE a = TE { unTE :: U.E } deriving (Eq, Show)

instance IsString (TE a) where
    fromString s = TE $ U.Var s

instance Num a => Num (TE a) where
	e1 + e2 = TE $U.M2 U.Add (unTE e1) (unTE e2)
	e1 - e2 = TE $U.M2 U.Sub (unTE e1) (unTE e2)
	e1 * e2 = TE $U.M2 U.Mul (unTE e1) (unTE e2)
        negate (TE (U.Const (NumV nv))) = TE $ U.Const $ NumV (negate nv)
	negate e = TE $ U.M2 U.Mul (U.Const . NumV . NInt $ (-1)) (unTE e)
	abs e = TE $U.If (U.Cmp U.Lt (unTE e) 0) (negate (unTE e)) (unTE e)
	signum e = TE $ U.If (U.Cmp U.Lt (unTE e) 0) (-1) (1)
	fromInteger i = TE $ U.Const . NumV $ NInt (fromInteger i)

instance Fractional a => Fractional (TE a) where
	e1 / e2 = TE $ U.M2 U.Div (unTE e1) (unTE e2)
	fromRational r = TE $ U.Const . NumV . NReal $ fromRational r

--instance Floating a => Floating (TE a) where


infixl 1 ~>                    

x ~> y = (x, y)            

            


--instance Show (TE a) where
--    show x= show $ unTE x

--instance Num a =>Num (TE a) where
--    fromInteger i = TE $ U.Const . NumV . NInt . fromInteger $ i

true, false :: TE Bool
true = TE . U.Const . BoolV $ True
false = TE . U.Const . BoolV $ False

iff :: TE Bool -> TE a -> TE a -> TE a
iff p c a = TE $ U.If (unTE p) (unTE c) (unTE a)

lam :: String -> TE b -> TE (a->b)
lam vn bod = --let vars = splitBySpaces vn in  .. addLams (reverse vars) 
             TE $ U.Lam vn (unTE bod)

($>) :: TE (a->b) -> TE a -> TE b
lm $> ag = TE $ U.App (unTE lm) (unTE ag)

nil :: TE [a]
nil = TE U.Nil

cons :: TE a -> TE [a] -> TE [a]
cons hd tl = TE $ U.Cons (unTE hd) (unTE tl)

list :: [TE a] -> TE [a]
list [] = nil
list (x:xs) = cons x $ list xs

pair :: TE a -> TE b -> TE (a,b)
pair x y = TE $ U.Pair (unTE x) (unTE y)

foo = iff true "x" "y"

bar :: TE Int
bar = (lam "x" "x") $> 1

--letE :: [(String, TE a)]
class TAssignable s where
    (~=) :: String -> TE a -> s

instance TAssignable U.Declare where
    x ~= y = let (nm:vars) = splitBySpaces x in 
             U.Let nm $ addLams (reverse vars) $ unTE y 

--instance Assignable (String, U.E) where -- LetE
--    x =: y = (x, y)

infixl 1 =:, ~=

s@t = U.SigAt t s

class Assignable s where
    (=:) :: String -> U.E -> s

instance Assignable U.Declare where
    x =: y = let (nm:vars) = splitBySpaces x in 
             U.Let nm $ addLams (reverse vars) y 

addLams [] e = e
addLams (v:vs) e = addLams vs $ U.Lam v e

instance Assignable (String, U.E) where -- LetE
    x =: y = (x, y)


splitBySpaces :: String -> [String]
splitBySpaces = filter (not. null) . splitBySpaces'

splitBySpaces' [] = []
splitBySpaces' (' ':s) = splitBySpaces s
splitBySpaces' s = let (hd, tl) = partition (/=' ') s 
                   in hd : splitBySpaces tl

