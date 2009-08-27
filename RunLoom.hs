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
        lov <- oneOf [0.04,0.02, 0.01]
	trace "displacement, lov" (dplc, lov)
	use "DisplacedLoom" ["angle" =: dbl dplc,"lov" =: dbl lov]
	runLoom ivl

isNumberStr str = all isDigit str

