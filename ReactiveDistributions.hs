{-# LANGUAGE TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, GADTs, BangPatterns #-}

module ReactiveDistributions where

import Math.Probably.FoldingStats
import Math.Probably.Distribution
import qualified Math.Probably.Sampler as S
import qualified Math.Probably.PDF as P
import NewSignal
import Numeric.LinearAlgebra
import TNUtils
import Data.Packed.Vector
import qualified Data.StorableVector as SV

sigToVector :: Signal Double -> Vector Double
sigToVector (Signal _ _ _ arr Eq) = fromList $ SV.unpack arr


data RandomSignal = RandomSignal (Signal Double) Double

instance Distribution RandomSignal where
    type Elem RandomSignal = Signal Double
    pdf (RandomSignal (Signal _ _ _ muArr Eq) noise) (Signal _ _ _ obsArr Eq) = 
      {-let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in \obsSig -> P.multiNormal mu sigma $ sigToVector $ forceSigEq obsSig -}
       SV.foldl1' (+) $ SV.zipWith (\muval obsVal -> P.logGaussD muval noise obsVal) muArr obsArr

instance ProperDistribution RandomSignal where
    sampler (RandomSignal meansig@(Signal t1 t2 dt _ _) noise) = 
      let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in do v <- S.multiNormal mu sigma
            return $ Signal t1 t2 dt (SV.pack $ toList v) Eq
    estimator = undefined

data RandomSignalFast = RandomSignalFast (Double->Double) Double

instance Distribution RandomSignalFast where
    type Elem RandomSignalFast = Signal Double
    pdf (RandomSignalFast meansigf noise) (Signal t1 t2 dt obsArr Eq)= 
      {-let k = round $ tmax/dt 
          mu = buildVector k $ meansigf . (*dt) . realToFrac
          sigma = (realToFrac noise) * ident k
      in \Signal _ _ _ arr Eq) -> sum P.multiNormal mu sigma $ sigToVector $ forceSigEq obsSig  -}
        let tmax = t2 - t1
            f i obsVal = P.logGaussD (meansigf . (*dt) . realToFrac $ i) noise obsVal
            facc (!sm, !i) obsVal = (sm+P.logGaussD (meansigf . (*dt) . realToFrac $ i) noise obsVal, i+1)
        in SV.foldl1' (+) $ SV.mapIndexed f obsArr

--buildVector :: Element a => Int -> (Int -> a) -> Vector a
                    
{-instance ProperDistribution RandomSignalFast where
    sampler (RandomSignal meansig@(Signal t1 t2 dt _ _) noise) = 
      let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in do v <- S.multiNormal mu sigma
            return $ Signal t1 t2 dt (SV.pack $ toList v) Eq
    estimator = undefined -}


data InhomogeneousPoisson = InhomogeneousPoisson (Signal Double)
instance Distribution InhomogeneousPoisson where
    type Elem InhomogeneousPoisson = [(Double,())]
    pdf (InhomogeneousPoisson rateSig) _ = 1

