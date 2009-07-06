{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Constraints where

import Control.Monad.State.Lazy
import Data.String
import qualified Data.List as L

data Atom t = Var String 
            | Const t 
            deriving (Show, Eq, Ord)

data Constraint t = Compare (Atom t) (t -> t -> Bool) (Atom t) 
                  | Unify (Atom t) (Atom t) (t -> t -> Maybe t) 
--                  deriving (Show, Eq, Ord)

instance Num t => Num (Atom t) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger x = Const $ fromInteger x

instance IsString (Atom t) where 
    fromString s = Var s

type Space t = [(String, t)]

type ConsM t a = StateT (Space t) [] a

getVal :: Atom t -> ConsM t t
getVal (Var k) = do dict <- get 
                    case lookup k dict of
                      Just v -> return v
                      Nothing -> mzero
getVal (Const x) = return x

putVal k v = do dict <- get
                put $ (k,v):dict

eval :: Constraint t -> ConsM t ()
eval (Compare e1 op e2) = lop2 (op) e1 e2

allVars :: Constraint t -> [String]
allVars (Compare e1 op e2) = atomVars e1 ++ atomVars e2


atomVars :: Atom t -> [String]
atomVars (Var s) = [s]
atomVars (Const _) = []

lop2 :: (t -> t -> Bool) -> Atom t -> Atom t -> ConsM t ()
lop2 op a1 a2 = do v1 <- getVal a1
                   v2 <- getVal a2
                   guard (v1 `op` v2)

solve :: [Constraint t] -> [t] -> [Space t]
solve es vals = (`execStateT` []) $ do 
                      forM nms $ \nm -> do
                        msum $ map (putVal nm) vals
                      sequence_ $ map eval es
                      return ()
    where nms = L.nub $ concatMap allVars es

test = solve ["x" .>. "y", 
              "y" .=. 1] 
       [0..10]

x .=. y = Compare x (==) y
x .>. y = Compare x (>) y

--assumes elem not present
--putAssoc :: Eq a => a -> b -> [(a, b)] -> [(a, b)] 
--putAssoc k v r = (k,v):r
