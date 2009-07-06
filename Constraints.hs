{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Constraints where

import Control.Monad.State.Lazy
import Data.String
--import qualified Data.List as L

data Atom = Var String 
          | Const Int 
            deriving (Show, Eq, Ord)

data Constraint = Atom :=: Atom 
                | Atom :>: Atom 
                | Atom :<: Atom
                  deriving (Show, Eq, Ord)

instance Num Atom where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger x = Const $ fromInteger x

instance IsString Atom where 
    fromString s = Var s

type Space = [(String, Int)]

type ConsM a = StateT Space [] a

getVal :: Atom -> ConsM Int
getVal (Var k) = do dict <- get 
                    case lookup k dict of
                      Just v -> return v
                      Nothing -> mzero
getVal (Const x) = return x

putVal k v = do dict <- get
                put $ (k,v):dict

eval :: Constraint -> ConsM ()
eval (e1 :=: e2) = lop2 (==) e1 e2
eval (e1 :>: e2) = lop2 (>) e1 e2
eval (e1 :<: e2) = lop2 (<) e1 e2

lop2 :: (Int -> Int -> Bool) -> Atom -> Atom -> ConsM ()
lop2 op a1 a2 = do v1 <- getVal a1
                   v2 <- getVal a2
                   guard (v1 `op` v2)

solve :: [Constraint] -> [String] -> [Int] -> [Space]
solve es nms vals = (`execStateT` []) $ do 
                      forM nms $ \nm -> do
                        msum $ map (putVal nm) vals
                      sequence_ $ map eval es
                      return ()

test = solve ["x" :>: "y", "y" :=: 1] ["x", "y"] [0..10]

--assumes elem not present
--putAssoc :: Eq a => a -> b -> [(a, b)] -> [(a, b)] 
--putAssoc k v r = (k,v):r
