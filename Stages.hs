module Stages where

import Expr
import Traverse

--data RelPos = At | Before | After

--type Target = Either String Int

-- 1. look for stage annotatinos
-- 2. for each anno, sigs it depends on get same stage anno

splitByStages :: [Declare] -> [[Declare]]
splitByStages ds = 
    let stages = [ s | Stage _ s <- ds ]
        (mainL, env) = partition declInMainLoop ds 
        stageDs st = let nms = [ nm | Stage nm s <- ds, s==st ]
                         in [ d | d@(Let nm _) <- ds, nm `elem` nms ]++
                            [ d | d@(SinkConnect (Var nm) _) <- ds, nm `elem` nms]
    in env : map stageDs stages