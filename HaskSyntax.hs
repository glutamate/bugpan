{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module HaskSyntax where

import qualified Expr as U
import EvalM
import Data.List

infixl 1 =:

class Assignable s where
    (=:) :: String -> U.E -> s

instance Assignable U.Declare where
    x =: y = let (nm:vars) = splitBySpaces x in 
             U.Let nm $ addLams (reverse vars) y 
        where addLams [] e = e
              addLams (v:vs) e = addLams vs $ U.Lam v e

instance Assignable (String, U.E) where -- LetE
    x =: y = (x, y)


splitBySpaces :: String -> [String]
splitBySpaces = filter (not. null) . splitBySpaces'

splitBySpaces' [] = []
splitBySpaces' (' ':s) = splitBySpaces s
splitBySpaces' s = let (hd, tl) = partition (/=' ') s 
                   in hd : splitBySpaces tl


class ToE a b where
    toE :: a-> TE b

instance ToE [Char] a where
    toE vn = TE (U.Var vn)

instance ToE (TE a) a where
    toE = id

infixl 1 ~>                    

x ~> y = (x, y)            

newtype TE a = TE { unTE :: U.E }

true, false :: TE Bool
true = TE . U.Const . BoolV $ True
false = TE . U.Const . BoolV $ False

ifE :: TE Bool -> TE a -> TE a -> TE a
ifE p c a = TE $ U.If (unTE p) (unTE c) (unTE a)

lam :: (ToE a b) => String -> a -> TE (b->c)
lam vn bod = TE $ U.Lam vn (unTE (toE bod))

-- ($>) :: TE (a->b) -> TE a -> TE B