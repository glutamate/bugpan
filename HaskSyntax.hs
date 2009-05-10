{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module HaskSyntax where

import qualified Expr as U
import EvalM
import Data.List
import Numbers

default (Int, Double)

infixl 1 =:

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


class ToE a b | b->a where
    toE :: a-> TE b

instance ToE [Char] a where
    toE vn = TE (U.Var vn)

instance ToE (TE a) a where
    toE = id

instance ToE Int Int where
    toE i = TE . U.Const . NumV . NInt $ i 

--instance ToE (TE Int) Int where
--    toE = id

infixl 1 ~>                    

x ~> y = (x, y)            

newtype TE a = TE { unTE :: U.E } deriving (Eq, Show)

--instance Show (TE a) where
--    show x= show $ unTE x

--instance Num a =>Num (TE a) where
--    fromInteger i = TE $ U.Const . NumV . NInt . fromInteger $ i

true, false :: TE Bool
true = TE . U.Const . BoolV $ True
false = TE . U.Const . BoolV $ False

iff :: (ToE p Bool, ToE c r, ToE a r) => p -> c -> a -> TE r
iff p c a = TE $ U.If (unTE . toE $ p) (unTE. toE $  c) (unTE. toE $  a)

lam :: (ToE a b) => String -> a -> TE (b->c)
lam vn bod = --let vars = splitBySpaces vn in  .. addLams (reverse vars) 
             TE $ U.Lam vn (unTE (toE bod))

($>) :: (ToE sab (a->b), ToE sa a) => sab -> sa -> TE b
lm $> ag = TE $ U.App (unTE . toE $ lm) (unTE . toE $ ag)

foo = iff true "x" "y"

bar :: TE Int
bar = (lam "x" "x") $> (1::Int)