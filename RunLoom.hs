module Main where

import Runner
import HaskSyntaxUntyped
import Expr
import EvalM
import Numbers
import System.Environment
import Data.Char
import Data.List
import TNUtils

main = do args <- getArgs
          let interval = (read `fmap` (safeLast $ filter isNumberStr args)) `orJust` 240
          go args $ runLoom (interval)

runLoom ivl = 
    do 	wait ivl
	dplc <- uniform (-0.1) (0.1)
	trace "displacement" dplc
	use "DisplacedLoom" ["angle" =: dbl dplc]
	runLoom ivl

isNumberStr str = all isDigit str

