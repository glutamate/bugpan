{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Proof.Equal where

import Baysig.Expr
import Baysig.Proof.Core
import Control.Monad
import Data.Maybe

apTerm tm th = refl tm >>= \ref -> mkApp ref th


