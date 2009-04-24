module Stages where

import Expr
import Traverse

data RelPos = At | Before | After

type Target = Either String Int