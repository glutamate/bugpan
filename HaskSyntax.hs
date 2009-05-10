{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleContexts, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

module HaskSyntax where

import qualified Expr as U
import EvalM
import Data.List
import Numbers

default (Int, Double)


class ToE a b | a->b where
    toE :: a-> TE b

instance ToE [Char] a where
    toE vn = TE (U.Var vn)

instance ToE (TE a) a where
    toE = id

instance (ToE a ta, ToE b tb) => ToE (a,b) (ta,tb) where
    toE (x,y) = TE (U.Pair (unTE . toE $ x) (unTE . toE $ y) )

instance (ToE a ta) => ToE [a] [ta] where
    toE [] = TE U.Nil
    toE (x:xs) = TE (U.Cons (unTE . toE $ x) (unTE . toE $ xs) )


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

nil :: TE [a]
nil = TE U.Nil

cons :: (ToE t te, ToE ttl [te]) => t -> ttl -> TE te
cons hd tl = TE $ U.Cons (unTE . toE $ hd) (unTE . toE $ tl)

foo = iff true "x" "y"

bar :: TE Int
bar = (lam "x" "x") $> (1::Int)



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

