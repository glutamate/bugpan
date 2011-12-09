{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction, ViewPatterns, PackageImports #-}
module QuantalHelp where
import Math.Probably.MCMC
import Math.Probably.StochFun
import Math.Probably.Sampler
import Math.Probably.FoldingStats
import Control.Monad
import Data.Array
import qualified Statistics.Math as SM
import qualified Math.Probably.Student as S
import qualified Data.StorableVector as SV
import Codec.Image.DevIL
import qualified Data.Array.Unboxed as UA
import Foreign.ForeignPtr
import Foreign.Storable.Tuple
import qualified Data.StorableVector as SV
import System.IO.Unsafe
import qualified Math.Probably.PDF as PDF
import qualified Numeric.LinearAlgebra as L
import "baysig" Baysig.Estimate.RTS
import Math.Probably.RandIO
import Data.List

neg = \x-> 0.000-x
square = \x-> x*x
step = \x-> if (x<0.000) then 0.000 else 1.000
fac = \_arg0-> case _arg0 of 1 -> 1; n -> n*(fac (n-1))
assert_fac3 = (fac 3)==6
assert_pairs0 = 2==(fst ((2,3)))
assert_pairs1 = 3==(fst ((2,3)))
assert_case = case 1 of 0 -> False; 1 -> True
oneOf = \xs-> ((fmap floor)$(uniform 0.000 (realToFrac$(length xs))))>>=(\idx-> return$(ix idx xs))
uniformLogPdf = \lo-> \hi-> \x-> 
   if ((x<hi)&&(x>lo)) 
       then 1.000 
       else {-trace ("expected "++show x++"to be in rng "++show (lo,hi)) -}(0.000-1.000e30)
uniform' = \lo-> \hi-> unit>>=(\x-> return ((x*(hi-lo))+lo))
unormal' = unit>>=(\u1-> unit>>=(\u2-> return ((sqrt ((0.000-2.000)*(log u1)))*(cos ((2.000*pi)*u2)))))
oneTo = \n-> (uniform 0.500 ((realToFrac n)+0.500))>>=(\x-> return (round x))
oneToLogPdf = \hi-> \x-> if ((x<(hi+1))&&(x>0)) then 1.000 else (0.000-1.000e20)
normal = \mean-> \tau-> unormal>>=(\u-> return ((u/(sqrt (tau*2.000)))+mean))
normalSd = \mean-> \sd-> unormal>>=(\u-> return ((u*sd)+mean))
normalPdf = \mu-> \tau-> \x-> (sqrt ((tau/2.000)*pi))*(exp (0.000-(((x-mu)*(x-mu))*tau)))
normalLogPdf :: Double -> Double -> Double -> Double
normalLogPdf = \mu-> \tau-> \x-> (log (sqrt ((tau/2.000)*pi)))+(0.000-(((x-mu)*(x-mu))*tau))
sdToTau = \sd-> 1.000/((2.000*sd)*sd)
meanCVToTau = \mn-> \cv-> 1.000/((2.000*(cv*mn))*(cv*mn))
varToTau = \var-> 1.000/(2.000*var)
tauToSD = \t-> 1.000/(sqrt (t*2.000))
logNormal = \mu-> \tau-> (fmap exp)$(normal mu tau)
logNormalPdf = \mu-> \tau-> \x-> ((sqrt tau)*(1.000/x))*(exp (0.000-(((tau*((log x)-mu))*((log x)-mu))/2.000)))
binNormalApprox = \n-> \p-> normal (n*p) (varToTau ((n*p)*(1.000-p)))
binomialProb = \n-> \p-> \k-> ((choose n k)*(p^k))*((1.000-p)^(n-k))
binomialLogProb :: Int ->Double ->Int -> Double 
binomialLogProb = \n-> \p-> \k-> ((log$(choose n k))+((realToFrac k)*(log p)))+((realToFrac (n-k))*(log (1.000-p)))
countTrue = \_arg0-> case _arg0 of Nil  -> 0; Cons True  bs -> 1+(countTrue bs); Cons False  bs -> countTrue bs
betaLogPdf = \a-> \b-> \x-> log$(((1.000/(S.beta (realToFrac a) (realToFrac b)))*(x^(a-1)))*((1.000-x)^(b-1)))
unfoldN = \n-> \m-> \lastx-> \s-> if (n<m) then ((s n lastx)>>=(\x-> (((unfoldN (n+1) m) x) s)>>=(\xs-> return (Cons x xs)))) else ((s n lastx)>>=(\v-> return (Cons v Nil)))
unfold = \n-> \lastx-> \s-> ((unfoldN 1 n) lastx) s
binGauss = \ns-> \p-> \q-> \cv-> \bgSd-> (binomial ns p)>>=(\nr-> normal ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd))))
binGaussPdf = \ns-> \p-> \q-> \cv-> \bgSd-> \v-> (bigSum 0 ns)$(\nr-> ((normalPdf ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd)))) v)*((binomialProb ns p) nr))
binGaussLogPdf = \ns-> \p-> \q-> \cv-> \bgSd-> \v-> log$((bigSum 0 ns)$(\nr-> exp$(((normalLogPdf ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd)))) v)+((binomialLogProb ns p) nr))))
normalInit = \mu-> \(_) -> mu
uniformInit = \lo-> \hi-> (hi+lo)/2.000
oneToInit = \n-> div n 2
binGaussInit = \n-> \p-> \q-> \(_) -> \(_) -> ((realToFrac n)*p)*q
betaInit = \a-> \b-> a/(a+b)
alpha = \tc-> \t-> ((((step t)*tc)*tc)*t)*(exp ((0.000-t)*tc))
qsig = \amp-> \tc-> \t0-> \off-> \t-> off+(amp*(alpha tc (t-t0)))
covOU = \theta-> \sigma-> \s-> \t-> (((sigma*sigma)*0.500)/theta)*(exp (0.000-(theta*(abs (s-t)))))
dt = 5.000e-5
tmax = 1.000e-1
np = round$(tmax/dt)
toD = \i-> (realToFrac i)*dt

ifObs = \i-> \j-> \sig-> if (i==j) then sig else 0.000

gpByInvLogPdf = \(_) -> \(_) -> \meansig-> \lndet-> \covinv-> \obssig-> let ((dt,_),obsvec) = observe obssig; meanVec = (fillV np)$(\i-> meansig (toD i)) in ((mvnPdf lndet covinv) meanVec) obsvec

posteriorNoiseV sigs v = let (covM)=fillM ((np,np)) (\(i,j)-> ((((sigma*sigma)*0.500)/(exp logtheta))*(exp (0.000-((exp logtheta)*(abs (((realToFrac i)*dt)-((realToFrac j)*dt)))))))+(if (i==j) then (exp logobs) else 0.000)) in let (inv,lndet)=invlndet covM in uniformLogPdf (0.000-50.000) (100.000) logtheta
 +uniformLogPdf (0.000) (10.000) sigma
 +uniformLogPdf (0.000-50.000) (100.000) logobs
 +uniformLogPdf (0.000-80.000) (0.000-40.000) vmean
 +(sum $ (flip map) (zip [1..10] sigs) $ \(i, sigv)->gpByInvLogPdf (dt) (tmax) (\y-> vmean) (lndet) (inv) sigv)
  where logtheta = v@> 0
        sigma = v@> 1
        logobs = v@> 2
        vmean = v@> 3

(@>) = (L.@>)

ouSynapseLogPdf (covinv, lndet) 
      = \meansig-> \obssig-> let (_,obsvec) = observe obssig
                                 (_,meanvec) = observe meansig
                             in ((mvnPdf lndet covinv) meanvec) obsvec

scaleSig off x (Signal dt t0 vec) = Signal dt t0 (L.mapVector (\v-> v*x+off) vec)

baselineSig :: Double -> Signal Double -> Signal Double
baselineSig tbase (Signal dt tst vec) = 
   let ntake = round $ tbase/dt
       basevec = L.subVector 0 ntake vec
       xsub = (L.foldVector (+) 0 basevec) / realToFrac ntake      
   in (Signal dt tst (L.mapVector (subtract xsub) vec))

posteriorSigV wf invDetails sig v 
  = ouSynapseLogPdf invDetails (scaleSig (v0) amp wf) sig
 where v0 = v@> 0
       amp = v@> 1

posteriorNPQV amps pcurve sd v = -- ((n,cv,slope,offset,phi,plo,q,tc,t0), loopvals) = 
 oneToLogPdf (400) n
 +uniformLogPdf 0.01 0.5 cv
 +uniformLogPdf (0) (1) phi
 +uniformLogPdf (0.000) (2.000e-1) q
 +(sum $ (flip map) (zip pcurve amps) $ \(pcurveVal, amp)->let p=pcurveVal * phi in binGaussLogPdf (n) (p) (q) (cv) (sd) amp)
  where n = round $ v @> 0
        cv = exp $ v @> 1
        phi = v @> 2
        q = exp $ v @> 3

cut = 500


weigh _ [] = []
weigh t_hat (pt@(t_data,y):rest) 
  | abs(t_data-t_hat) > cut = weigh t_hat rest
  | otherwise = replicate (5 - (floor $ abs(t_data-t_hat)/cut * 5)) pt ++ weigh t_hat rest


weighRegression :: [(Double,Double)] -> Double -> Double
weighRegression pts x = y where
   myPts = weigh x pts -- filter (\(t,y)-> abs(x-t) < cut) pts
   (slope,offset) = runStat regressF myPts
   y = slope*x + offset

zscore :: [(Double,Double)] -> (Double,Double) -> Double
zscore tsamps (t,amp) = abs z where
  relevant = filter ((< 200) . abs . (`subtract` t) . fst) tsamps
  (slope, offset) = runStat regressF $ relevant
  detrended = map (\(t,amp)-> amp - (slope*t + offset)) relevant
  (mean, sd) = runStat meanSDF $ detrended
  z = (amp - (slope*t + offset))/ sd
 

showNPQV :: L.Vector Double -> String
showNPQV (L.toList -> [n, logcv, phi, logq]) 
  = intercalate "\t" $ map (minWidth 8 . accushow) [n, exp logcv, phi, exp logq]

minWidth n s | len < n = s ++ replicate (n - len) ' '
             | otherwise = s
  where len = length s

simslope = 8.000e-3
simq = 2.000e-4
--simn = 50
simoffset = 500
simntrials = 1000
simt0 = 0.035


thetaHat = 0.15 --4.630e-3
sigmaHat = 1.6 --6.260
obsHat = 2.7e-4


fakesam simn = (return simn)>>=(\n-> (return 0.150)>>=(\cv-> (return simslope)>>=(\slope-> (return simoffset)>>=(\offset-> (return 0.800)>>=(\phi-> (return 0.200)>>=(\plo-> (return (simq*(100/realToFrac simn)))>>=(\q-> (return 170.000)>>=(\tc-> (return simt0)>>=(\t0-> (for 1 simntrials)$(\i-> let p = phi-((phi-plo)/(1.000+(exp ((offset*slope)-(slope*(realToFrac i)))))) in ((((binGauss n p) q) cv) 0.000)>>=(\amp-> (return (0-60.000))>>=(\vstart-> ((gpByChol dt tmax) (\t-> vstart+(amp*(((((step (t-t0))*tc)*tc)*(t-t0))*(exp ((0.000-(t-t0))*tc)))))) cholm))))))))))))
  where cholm = chol$((fillM (((np+1),(np+1))))$(\(i,j)-> ((covOU thetaHat sigmaHat) (toD i)) (toD j)+ifObs i j obsHat))

stagger (i, Signal dt t0 sig) = Signal dt i sig