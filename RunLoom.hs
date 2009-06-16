module Main where

import Runner
import HaskSyntaxUntyped
import Expr
import EvalM
import Numbers


main = go runLoom

runLoom = do 	wait 5
		dplc <- uniform (-0.1) (0.1)
		trace "displacement" dplc
		use "DisplacedLoom" ["angle" =: dbl dplc]
		runLoom
