module RandomSources where

import System.Random
import Control.Monad

poissonKnuth :: Double -> IO Int
poissonKnuth rate = pvAux 0 1
    where pvAux k p = do u <- randomRIO (0.0,1.0)
                         let p' = p*u
                         if p'>l
                            then pvAux (k+1) p'
                            else return k
          l = exp (-rate)



{- 
http://en.wikipedia.org/wiki/Poisson_distribution 
algorithm poisson random number (Knuth):
    init:
         Let L ← e−λ, k ← 0 and p ← 1.
    do:
         k ← k + 1.
         Generate uniform random number u in [0,1] and let p ← p × u.
    while p > L.
    return k − 1.
-}

-- www.maths.lth.se/matstat/kurser/fms180mas204/lab2.pdf
-- -(log(1-u))/lambda

poisson1 :: Double -> IO Double
poisson1 rate = do u <- randomRIO (0.0,1.0)
                   return $ -(log(1-u))/rate

poisson :: Double -> Double -> Double -> IO [(Double,())]
poisson rate tmax dt = aux 0
    where aux last = do next <- (+last) `fmap` poisson1 rate
                        if next > tmax 
                           then return []
                           else liftM2 (:) (return (next, ())) $ aux next
