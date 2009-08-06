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


enowAux t dt evs = let nstep t dt = round (t/dt)
                       dropF (te,_) = nstep te dt > nstep t dt
                       takeF (te,_) = nstep te dt == nstep t dt
                   in takeWhile takeF $ dropWhile dropF evs

selSwitch :: [([(Double, a)], Double -> a -> b)] -> b -> b
selSwitch eslams def = selSwitch' eslams def 0

selSwitch' [] def _ = def
selSwitch' (([], _):es) def t = selSwitch' es def t
selSwitch' (((tev,x):_, f):es) def t | tev > t = selSwitch' es (f tev x) tev
                                     | otherwise = selSwitch' es def t


sigTmax :: Signal a -> Double
sigTmax (Signal t1 t2 dt sf) = t2