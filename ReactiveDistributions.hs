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
import QueryTypes
import Database
import Data.Maybe

import Control.Monad
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
    --pdf (RandomSignalFast meansigf noise) (Signal t1 t2 dt obsArr Eq)= 
      {-let k = round $ tmax/dt 
          mu = buildVector k $ meansigf . (*dt) . realToFrac
          sigma = (realToFrac noise) * ident k
      in \Signal _ _ _ arr Eq) -> sum P.multiNormal mu sigma $ sigToVector $ forceSigEq obsSig  -}
    pdf = altPdfRSF1 -- longPdfRSF

altPdfRSF1 :: RandomSignalFast -> Signal Double -> Double
altPdfRSF1 (RandomSignalFast meansigf noise) (Signal t1 t2 dt obsArr Eq)= 
        let f i obsVal = P.logGaussD (meansigf . (*dt) . realToFrac $ i) noise obsVal
        in SV.foldl1' (+) $ SV.mapIndexed f obsArr

altPdfRSF :: RandomSignalFast -> Signal Double -> Double
altPdfRSF (RandomSignalFast meansigf noise) (Signal t1 t2 dt obsArr Eq)= 
        let facc  obsVal (sm, i) = (sm+P.logGaussD (meansigf . (*dt) . realToFrac $ i) noise obsVal, i+1)
        in fst $ SV.foldr facc (0,0) obsArr

longPdfRSF :: RandomSignalFast -> Signal Double -> Double
longPdfRSF  (RandomSignalFast meansigf noise) obsSig@(Signal t1 t2 dt obsArr Eq)= 
      let meansig = fillSig t1 t2 dt meansigf
          mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in log $ P.multiNormal mu sigma $ sigToVector $ forceSigEq obsSig 


testw = fillSig 0 2 0.001 id
testRS = RandomSignalFast id 0.1

testm = (realToFrac 3.2 * ident 10):: Matrix Double

--buildVector :: Element a => Int -> (Int -> a) -> Vector a
                    
{-instance ProperDistribution RandomSignalFast where
    sampler (RandomSignal meansig@(Signal t1 t2 dt _ _) noise) = 
      let mu = sigToVector $ forceSigEq meansig
          k = dim mu
          sigma = (realToFrac noise) * ident k
      in do v <- S.multiNormal mu sigma
            return $ Signal t1 t2 dt (SV.pack $ toList v) Eq
    estimator = undefined -}


data InhomogeneousPoisson = InhomogeneousPoisson (Signal Double) (Signal Double) 

instance Distribution InhomogeneousPoisson where
    type Elem InhomogeneousPoisson = [(Double,())]
    pdf (InhomogeneousPoisson rateSig intRate ) evs = 
        (sum $ map (log . (rateSig `readSig`) . fst) evs) -log (intRate`readSig`(sigT1 rateSig) - intRate`readSig`(sigT2 rateSig))


instance ProperDistribution InhomogeneousPoisson where
    sampler (InhomogeneousPoisson rateSig _) = sIPevSam rateSig
    estimator = undefined


simulateInhomogeneousPoisson ::[Duration a] -> (a -> Signal Double) -> S.Sampler [Event ()]
simulateInhomogeneousPoisson durpars condRate = 
    let bigsam = fmap concat $ forM durpars $ \((t1d, t2d),p)-> do
                    evs <- sIPevSam $ condRate p
                    return $ map (onFst (+t1d)) evs
    in  bigsam

sIPevSam :: Signal Double -> S.Sampler [Event ()]
sIPevSam rate@(Signal t1 t2 dt _ _) = do
{-  fmap catMaybes $ forM (sigTimePoints rate) $ \t-> do
                          u <- S.unitSample
                          if u<(rate `readSig` t) * dt
                             then return $ Just (t,())
                             else return Nothing -}
  let isGo t = do u <- S.unitSample
                  return $ u<(rate `readSig` t) * dt
  fmap (map $ flip (,) ()) $ filterM isGo (sigTimePoints rate)

--foo = filterM 

test = simulateInhomogeneousPoisson [((10,13), 11), ((20,23), 20)] (\x-> x*sineSig)

