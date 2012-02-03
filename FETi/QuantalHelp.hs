{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction, ViewPatterns, PackageImports #-}
module QuantalHelp where
import "probably" Math.Probably.MCMC
import "probably" Math.Probably.StochFun
import "probably" Math.Probably.Sampler
import "probably" Math.Probably.FoldingStats
import Control.Monad
import Data.Array
--import qualified Statistics.Math as SM
import qualified Math.Probably.Student as S
import qualified Data.StorableVector as SV
--import Codec.Image.DevIL
import qualified Data.Array.Unboxed as UA
import Foreign.ForeignPtr
import Foreign.Storable.Tuple
import qualified Data.StorableVector as SV
import System.IO.Unsafe
import qualified Math.Probably.PDF as PDF
import qualified Numeric.LinearAlgebra as L
import "baysig" Baysig.Estimate.RTS
import "probably" Math.Probably.RandIO
import "probably" Math.Probably.NelderMead
import Data.List

import Query hiding (io) 
import QueryTypes
import QueryUtils hiding (averageSigs)

import Debug.Trace

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
oneToLogPdf = \hi-> \x-> if ((x<(hi+1))&&(x>0)) then 1.000 else (0.000-1.000e50)
normal = \mean-> \tau-> unormal>>=(\u-> return ((u/(sqrt (tau*2.000)))+mean))
normalSd = \mean-> \sd-> unormal>>=(\u-> return ((u*sd)+mean))
normalPdf = \mu-> \tau-> \x-> (sqrt ((tau/2.000)*pi))*(exp (0.000-(((x-mu)*(x-mu))*tau)))
normalLogPdf :: Double -> Double -> Double -> Double
normalLogPdf = \mu-> \tau-> \x-> (log (sqrt ((tau/2.000)*pi)))+(0.000-(((x-mu)*(x-mu))*tau))
sdToTau = \sd-> 1.000/((2.000*sd)*sd)
meanCVToTau = \mn-> \cv-> 1.000/((2.000*(cv*mn))*(cv*mn))
varToTau :: Double -> Double
varToTau = \var-> 1.000/(2.000*var)
tauToSD = \t-> 1.000/(sqrt (t*2.000))
logNormal = \mu-> \tau-> (fmap exp)$(normal mu tau)
logNormalPdf = \mu-> \tau-> \x-> ((sqrt tau)*(1.000/x))*(exp (0.000-(((tau*((log x)-mu))*((log x)-mu))/2.000)))
binNormalApprox = \n-> \p-> normal (n*p) (varToTau ((n*p)*(1.000-p)))
binomialProb = \n-> \p-> \k-> ((choose n k)*(p^k))*((1.000-p)^(n-k))
binomialLogProb :: Int ->Double ->Int -> Double 
binomialLogProb = \n-> \p-> \k-> ((log$(choose n k))+((realToFrac k)*(log p)))+((realToFrac (n-k))*(log (1.000-p)))

binomialLogProb' n p k | k > n = binomialLogProb' n p n 
                       | k < 0 = binomialLogProb' n p 0
                       | n < 0 = error "n zero"
                       | otherwise = ((log$(choose n k))+((realToFrac k)*(log p)))+((realToFrac (n-k))*(log (1.000-p)))

countTrue = \_arg0-> case _arg0 of Nil  -> 0; Cons True  bs -> 1+(countTrue bs); Cons False  bs -> countTrue bs
betaLogPdf = \a-> \b-> \x-> log$(((1.000/(S.beta (realToFrac a) (realToFrac b)))*(x^(a-1)))*((1.000-x)^(b-1)))
unfoldN = \n-> \m-> \lastx-> \s-> if (n<m) then ((s n lastx)>>=(\x-> (((unfoldN (n+1) m) x) s)>>=(\xs-> return (Cons x xs)))) else ((s n lastx)>>=(\v-> return (Cons v Nil)))
unfold = \n-> \lastx-> \s-> ((unfoldN 1 n) lastx) s
binGauss = \ns-> \p-> \q-> \cv-> \bgSd-> (binomial ns p)>>=(\nr-> normal ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd))))
binGaussPdf = \ns-> \p-> \q-> \cv-> \bgSd-> \v-> (bigSum 0 ns)$(\nr-> ((normalPdf ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd)))) v)*((binomialProb ns p) nr))

binGaussLogPdf = \ns-> \p-> \q-> \cv-> \bgSd-> \v-> log$((bigSum 0 ns)$(\nr-> exp$(((normalLogPdf ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd)))) v)+((binomialLogProb ns p) nr))))

binGaussFrom1LogPdf ns p q  cv bgSd v = log$((bigSum 1 ns)$(\nr-> exp$(((normalLogPdf ((realToFrac nr)*q) (varToTau$(((((q*cv)*q)*cv)*(realToFrac nr))+(bgSd*bgSd)))) v)+((binomialLogProb ns p) nr))))


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
       amp = v@>1       

posteriorNPQV amps pcurve sd v = -- ((n,cv,slope,offset,phi,plo,q,tc,t0), loopvals) = 
 oneToLogPdf (800) n
 +uniformLogPdf 0.00001 0.5 cv
 +uniformLogPdf (0) (1) phi
 +uniformLogPdf (0.000) (1) q
 +(sum $ (flip map) (zip pcurve amps) $ \(pcurveVal, amp)->
               let p=pcurveVal * phi 
               in binGaussLogPdf (n) (p) (q) (cv) (sd) amp)
  where n = round $ v @> 0
        cv = exp $ v @> 1
        phi = v @> 2
        q = exp $ v @> 3

type Vec = L.Vector Double

fastNPQ :: (Int -> Vec -> Double) -> Int -> Vec -> (Vec, Int, Double, Simplex)
fastNPQ pdfN n0 par0 = fN initLike (n0-1) par0 where
  initLike = -1e20 -- fst $ optimise n0 par0

  fN lastLike nlast pars = let (thislike, thispars, simp) = optimise (nlast+1) pars
                           in if thislike > lastLike 
                                 then fN thislike (nlast+1) thispars
                                 else (pars, nlast, lastLike, simp)
  optimise n pars = let pdf = pdfN n
                        (maxV, _,smplx) = laplaceApprox defaultAM {nmTol = 0.1} pdf [] pars
                        --(maxPost,hess) = hessianFromSimplex (negate . pdfN) [] $ augmentSimplex n smplx 
                        like = pdf maxV
                      in  trace ("laplace"++show n++": "++show maxV++" lh="++show like) $ (like, maxV, smplx)

       
augmentSimplex n = map f where
   f (vec, like) = (L.join [morev, vec], like)
   morev = L.fromList [realToFrac n]
       


                  
hessianFromSimplex' :: (L.Vector Double -> Double) -> [Int] -> Simplex -> (L.Vector Double, Matrix Double)
hessianFromSimplex' f isInt sim = 
  let mat :: [L.Vector Double]
      mat = L.toRows $ L.fromColumns $ map fst sim
      fsw ((y0, ymin),ymax) = (y0, max (ymax-y0) (y0-ymin))
      swings = flip map mat $ runStat (fmap fsw $ meanF `both` minFrom 1e80 `both` maxFrom (-1e80)) . L.toList 
      n = length swings
      xv = L.fromList $ map fst swings
      fxv = f  xv
      iswings i | i `elem` isInt = atLeastOne' $snd $ swings!!i
                | otherwise = snd $ swings!!i
      funits d i | d/=i = 0
                 | i `elem` isInt = atLeastOne' $ snd $ swings!!i
                 | otherwise = snd $ swings!!i 
      units = traceit "\n\nunits" $ flip map [0..n-1] $ \d -> L.buildVector n $ funits d
      --http://www.caspur.it/risorse/softappl/doc/sas_docs/ormp/chap5/sect28.htm
      fhess (i,j) | i>=j = 
                      ((f $ xv + units!!i + units!!j)
                       - (f $ xv + units!!i - units!!j)
                       - (f $ xv - units!!i + units!!j)
                       + (f $ xv - units!!i - units!!j) ) 
                      / (4*(iswings i) * (iswings j))
                  | otherwise = 0.0    
      hess1= L.buildMatrix n n fhess 
      hess2 = L.buildMatrix n n $ \(i,j) ->if i>=j then hess1 L.@@>(i,j) 
                                                 else hess1 L.@@>(j,i) 
  in (L.fromList (map (fst) swings), hess2)


setM i j v m = L.buildMatrix (L.rows m) (L.cols m)$ \(i',j') ->if i'==i && j==j' then v
                                                               else m L.@@>(i',j') 



atLeastOne' :: Double -> Double
atLeastOne' x | isNaN x || isInfinite x = 1.0
              | x < -1.0 || x > 1.0 = realToFrac $ round x
              | x < 0 = -1.0
              | otherwise = 1.0



traceit s x = trace (s++": "++show x) x

setN x v = L.buildVector 6 $ \ix -> if ix == 0 then x else v L.@> ix

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
 

showNPQV' am = showNPQV (ampPar am)++" lh: "++show (lastLike am)

showNPQV :: L.Vector Double -> String
showNPQV (L.toList -> [n, logcv, phi, logq]) 
  = intercalate "\t" $ map (minWidth 8 . accushow) [n, exp logcv, phi, exp logq]
  

minWidth n s | len < n = s ++ replicate (n - len) ' '
             | otherwise = s
  where len = length s

simq = 4.000e-4
--simn = 50

--simntrials = 1000
simt0 = 0.035


thetaHat = 0.15 --4.630e-3
sigmaHat = 1.6 --6.260
obsHat = 2.7e-4


fakesam simn ntrials = (return simn)>>=(\n-> (return 0.150)>>=(\cv-> (return sslope)>>=(\slope-> (return soffset)>>=(\offset-> (return 0.400)>>=(\phi-> (return 0.200)>>=(\plo-> (return (simq*(100/realToFrac simn)))>>=(\q-> (return 170.000)>>=(\tc-> (return simt0)>>=(\t0-> (for 1 ntrials)$(\i-> let p = phi-((phi-plo)/(1.000+(exp ((offset*slope)-(slope*(realToFrac i)))))) in ((((binGauss n p) q) cv) 0.000)>>=(\amp-> (return (0-60.000))>>=(\vstart-> ((gpByChol dt tmax) (\t-> vstart+(amp*(((((step (t-t0))*tc)*tc)*(t-t0))*(exp ((0.000-(t-t0))*tc)))))) cholm))))))))))))
  where cholm = chol $ fillM (np+1,np+1) $ \(i,j)-> covOU thetaHat sigmaHat (toD i) (toD j)+ifObs i j obsHat
        soffset = ntrials / 2
        sslope = 8.000e-3 * (1000/ntrials)

stagger (i, Signal dt t0 sig) = Signal dt i sig

pad (c:[]) = '0':c:[]
pad cs = cs

fst3 (x,_,_) = x

whenContinues sess mma = do
      conts <- durations "continues" "foo"
      sessid <- getSessionName
      case conts of
        [] -> if (sessid==sess) then mma else return Nothing
        (_,s):_ | s `isPrefixOf` sess -> mma
                | otherwise -> return Nothing
  
startNs = [("00c9", 50), ("84", 40), ("512", 50) ]
filters = [("b34", \(t,amp)-> t>5300)]



getStartN sess = case find (\(s,v) -> s `isPrefixOf` sess) startNs of
                  Just (s,v) -> v
                  Nothing -> 30

getFilter sess = case find (\(s,v) -> s `isPrefixOf` sess) filters of
                  Just (s,v) -> v
                  Nothing -> const True

posteriorTop pcurve amparloops v = -- ((n,cv,slope,offset,phi,plo,q,tc,t0), loopvals) = 
 oneToLogPdf (800) n
 +uniformLogPdf 0.00001 0.5 cv
 +uniformLogPdf (0) (1) phi
 +uniformLogPdf (0.000) (1) q
 +(sum $ (flip map) (zip pcurve  amps) $ \(pcurveVal, amp)->
    let p=pcurveVal * phi 
        --nr::Int = round $ relfrac *p* realToFrac n
    in binGaussFrom1LogPdf n p q cv 0 amp) --  + -- binomialLogProb (n) (p) nr +
       --normalLogPdf (realToFrac nr*q) (varToTau (q*cv*q*cv*(realToFrac nr))) amp )
  where n = round $ v @> 0
        cv = exp $ v @> 1
        phi = v @> 2
        q = exp $ v @> 3
        --relfracs = map ( (@>0) .  ampPar) amparloops
        amps =map ( (@>0) .  ampPar) amparloops



posteriorLoop wf invDetails ampartop pcurveVal sig v 
  = binomialLogProb (n) (p) nr +
    normalLogPdf (realToFrac nr*q) (varToTau (q*cv*q*cv*(realToFrac nr))) amp +
    ouSynapseLogPdf invDetails (scaleSig (v0) amp wf) sig
 where v0 = v@> 2
       amp = v@>1       
       nr = round $ v@>0
       vt = ampPar ampartop
       n = round $ vt @> 0
       cv = exp $ vt @> 1
       phi = vt @> 2
       q = exp $ vt @> 3
       p = pcurveVal * phi
 
posteriorLoop' sd amtop pcurveVal sigAmpMean v 
  = binGaussFrom1LogPdf n p q cv 0 amp + --binomialLogProb' (n) (p) nr +
--    normalLogPdf (realToFrac nr*q) (varToTau (q*cv*q*cv*(realToFrac nr))) amp +
    --ouSynapseLogPdf invDetails (scaleSig (v0) amp wf) sig
    normalLogPdf sigAmpMean (varToTau $ sd*sd ) amp
 where --v0 = v@> 2
       amp = v@>0       
       --relfrac =  v@>0
       --nr = round $ relfrac *p* realToFrac n
       vtop = ampPar amtop
--       vQCV = ampPar amtopQCV
       n = round $ vtop @> 0
       cv = exp $ vtop @> 1
       phi = vtop @> 2
       q = exp $ vtop @> 3
       p = pcurveVal * phi
       
updateG wf invDetails pcurve sigs (ampartop,amparloops) = do
   --DONT FORGET NOT TO CACHE PREVIOUS POSTERIOR VALUE
   newtop <- adaMetNoCacheP False (posteriorTop pcurve amparloops) ampartop
   newloops <- forM (zip3 pcurve sigs amparloops) $ \(pcurveVal, sig, ampar) ->
                 adaMetNoCacheP False 
                        (posteriorLoop wf invDetails newtop pcurveVal sig) 
                        ampar
   return (newtop, newloops)

updateG' sd amps pcurve topcool (amtop, amparloops) = do
   --DONT FORGET NOT TO CACHE PREVIOUS POSTERIOR VALUE
   newtop <- adaMetNoCacheP False ((/topcool) . posteriorTop pcurve amparloops) amtop
   --newtopQCV <- adaMetNoCacheP False ((/topcool) . posteriorTopQCV pcurve newtopNP amparloops) amtopQCV
   newloops <- forM (zip3 pcurve amps amparloops) $ \(pcurveVal, amp, ampar) ->
                 adaMetNoCacheP False 
                        (posteriorLoop' sd newtop pcurveVal amp ) 
                        ampar
   return (newtop, newloops)


runGibbs 0 wf invDetails pcurve sigs (ampartop,amparloops) xs = return $ reverse xs
runGibbs n wf invDetails pcurve sigs (ampartop,amparloops) xs = do
      (newtop, newloops)<- updateG wf invDetails pcurve sigs (ampartop,amparloops)
      runGibbs (n-1) wf invDetails pcurve sigs (newtop, newloops) (ampPar newtop:xs)

runGibbs' [] sd amps pcurve pars xs = return $ reverse xs
runGibbs' ((0,_):rest) sd amps pcurve pars xs = do
      runGibbs' (rest) sd amps pcurve pars xs
runGibbs' ((n,topcool):rest) sd amps pcurve pars xs = do
      newpars@(p1,p3)<- updateG' sd amps pcurve topcool pars
      runGibbs' ((n-2,topcool):rest) sd amps pcurve newpars
                (L.join (map ampPar [p1,head p3]):xs)


reset_counts n ampar = ampar { count = n, count_accept = n `div` 2} 

shrink x ampar = ampar {ampCov = L.scale (recip x) $ ampCov ampar} 


{-posteriorTopNP pcurve amTopQCV amparloops v = -- ((n,cv,slope,offset,phi,plo,q,tc,t0), loopvals) = 
 oneToLogPdf (800) n
 +uniformLogPdf (0) (1) phi
 +(sum $ (flip map) (zip3 pcurve relfracs amps) $ \(pcurveVal, relfrac, amp)->
    let p=pcurveVal * phi 
        nr::Int = round $ relfrac *p* realToFrac n
    in binomialLogProb' (n) (p) nr)
  where n = round $ v @> 0
        --cv = exp $ v @> 1
        phi = v @> 1
        --q = exp $ v @> 3
        relfracs = map ( (@>0) .  ampPar) amparloops
        amps =map ( (@>1) .  ampPar) amparloops

posteriorTopQCV pcurve amTopNP amparloops v = -- ((n,cv,slope,offset,phi,plo,q,tc,t0), loopvals) = 
 normalLogPdf (0.2) (varToTau $ 0.05*0.05)cv
 +uniformLogPdf (0.000) (1) q
 +sm
  where n = round $ (ampPar amTopNP) @> 0
        cv = exp $ v @> 0
        phi = (ampPar amTopNP) @> 1
        q = exp $ v @> 1
        relfracs = map ( (@>0) .  ampPar) amparloops
        amps =map ( (@>1) .  ampPar) amparloops
        sm = (sum $ (flip map) (zip3 pcurve relfracs amps) $ \(pcurveVal, relfrac, amp)->
                let p=pcurveVal * phi 
                    nr::Int = round $ relfrac *p* realToFrac n
                    fakecv= 0.2
                    it = normalLogPdf (realToFrac nr*q) (varToTau (q*fakecv*q*fakecv*(realToFrac nr))) amp 
                in it ) -- `seq` if isNaN it then trace (show (p, relfrac, amp, nr)) it else it)
-}