module HaskShapes where

import Data.IORef
import EvalM
import Data.Array

data Shape a = Box ((a,a), a)
             | Translate ((a,a), a) (Shape a)
             | Colour ((a,a), a) (Shape a)

box = Box
translate = Translate
colour = Colour

appendIORef ref x = do xs <- readIORef ref
                       writeIORef ref (x:xs)



bufToSig buf dt tmax nsteps = 
    let arr = array (0,nsteps) $ zip [0..nsteps] $ reverse buf
    in Signal 0 tmax dt  $ \t-> arr!t