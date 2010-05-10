{-# LANGUAGE TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, GADTs #-}

module ReactiveDistributions where

import Math.Probably.FoldingStats
import Math.Probably.Distribution
import qualified Math.Probably.Sampler as S
import qualified Math.Probably.PDF as P
import NewSignal
import Numeric.LinearAlgebra
import TNUtils
import qualified Data.StorableVector as SV

sigToVector :: Signal Double -> Vector Double
sigToVector (Signal _ _ _ arr Eq) = fromList $ SV.unpack arr


data RandomSignal = RandomSignal (Signal Double) Double

instance Distribution RandomSignal where
    type Elem RandomSignal = Signal Double
    pdf (RandomSignal meansig noise) = 
      let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in \obsSig -> P.multiNormal mu sigma $ sigToVector $ forceSigEq obsSig

instance ProperDistribution RandomSignal where
    sampler (RandomSignal meansig@(Signal t1 t2 dt _ _) noise) = 
      let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in do v <- S.multiNormal mu sigma
            return $ Signal t1 t2 dt (SV.pack $ toList v) Eq
    estimator = undefined

data InhomogeneousPoisson = InhomogeneousPoisson (Signal Double)
instance Distribution InhomogeneousPoisson where
    type Elem InhomogeneousPoisson = [(Double,())]
    pdf (InhomogeneousPoisson rateSig) _ = 1

