{-# LANGUAGE GADTs #-}

module NewSignal where

--import 
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as SVST
import Foreign.Storable
import EvalM
import Foreign.Storable.Tuple
import Data.List
import Control.Monad.ST
import Control.Monad
import TNUtils

data EqOrK a b where
    Eq :: EqOrK a a
    Kont :: (a->b) -> EqOrK a b

eqOrKToF :: EqOrK a b -> (a->b)
eqOrKToF (Eq) = id
eqOrKToF (Kont f) = f

data Signal a where
    Signal :: Storable a => Double -> Double -> Double -> SV.Vector a -> EqOrK a b -> Signal b
    ConstSig :: a -> Signal a

sigArr :: (Storable a) => Signal a -> SV.Vector a
sigArr (Signal t1 t2 dt arr (Kont f)) = SV.map (f) arr
sigArr (Signal t1 t2 dt arr Eq) = arr

sigT1 (Signal t1 t2 dt arr eqOrK ) = t1
sigT2 (Signal t1 t2 dt arr eqOrK ) = t2
sigDT (Signal t1 t2 dt arr eqOrK ) = dt

forceSigEq :: (Storable a) => Signal a -> Signal a 
forceSigEq s@(ConstSig x) = s
forceSigEq s@(Signal _ _ _ _ Eq) = s
forceSigEq (Signal t1 t2 dt arr (Kont f)) =let arr' = SV.map f arr
                                           in Signal t1 t2 dt arr' Eq

instance Functor Signal where
    fmap f (ConstSig x) = ConstSig $ f x
    fmap f (Signal t1 t2 dt arr Eq) = Signal t1 t2 dt arr $ Kont f
    fmap f (Signal t1 t2 dt arr (Kont g)) = Signal t1 t2 dt arr $ Kont $ f . g

smap f = map (fmap f)

sigPnts :: Signal a -> Int
sigPnts (Signal t1 t2 dt arr _) = SV.length arr
sigVPnts (SigV t1 t2 dt sf ) = round $ (t2-t1)/dt

sigTimePoints s@(Signal t1 t2 dt _ _) = let n = sigPnts s
                                        in map ((+t1) . (*dt) . realToFrac) [0..n-1]

timePointsFromT1T2Dt t1 t2 dt = let n = round $ (t2-t1)/dt
                                in map ((+t1) . (*dt) . realToFrac) [0..n-1]

sigVTimePoints (SigV t1 t2 dt _) = let n = (t2-t1)/dt
                                   in map ((+t1) . (*dt)) [0..n-1]


sigToList :: Signal a -> [a]
sigToList sig@(Signal t1 t2 dt arr Eq) = SV.unpack arr 
sigToList sig@(Signal t1 t2 dt arr (Kont f)) = map f $ SV.unpack arr

sigInitialVal s  = head $ sigToList s

sscan :: (Storable a) => (a->b->a) -> a -> Signal b -> Signal a
sscan f init s@(Signal t1 t2 dt arr Eq) =
    Signal (sigT1 s) (sigT2 s) (sigDT s) (SV.scanl f init arr) Eq
sscan f init s@(Signal t1 t2 dt arr (Kont k)) =
    Signal (sigT1 s) (sigT2 s) (sigDT s) (SV.scanl (\last next ->f last (k next)) init arr) Eq
                             

zipWithTime :: Storable a => Signal a -> Signal (a,Double)
zipWithTime s@(Signal t1 t2 dt arr Eq) = 
    let zarr = SV.zipWith (,) ( arr) $ SV.pack (sigTimePoints s)
    in Signal t1 t2 dt zarr Eq 
zipWithTime sig = zipWithTime $ forceSigEq sig
           

foldSig :: (a->b->a) -> a -> Signal b -> a
foldSig f init s@(Signal _ _ _ arr Eq) = SV.foldl' f init arr
foldSig f init s@(Signal _ _ _ arr (Kont k)) = SV.foldl' (\x v -> f x (k v)) init arr
foldSig f init s@(ConstSig x) = f init x



instance Show a =>  Show (Signal a) where
    show sig@(Signal t1 t2 dt arr _) = 
        "{"++show t1++": "++(show . take 5 $ sigToList sig)++"... :"++show t2++"}"
    show (ConstSig x) = "ConstSig "++show x

readSig :: Signal a -> Double -> a
readSig (Signal t1 t2 dt arr eqOrK)  t = 
    (eqOrKToF eqOrK) $ arr `SV.index` (round $ (t-t1 )/dt)

readSigPt :: Signal a -> Int -> a
readSigPt (Signal t1 t2 dt arr eqOrK)  t = 
    (eqOrKToF eqOrK) $ arr `SV.index` t

sigInit tm s@(Signal t1 t2 dt arr eqOrK) = 
    let t2' = t1+tm
    in limitSig t1 t2' s
       
sigZero s@(Signal t1 t2 dt arr eqOrK) = Signal 0 (t2-t1) dt arr eqOrK


limitSig lo hi (Signal t1 t2 dt arr eqOrK) = 
    let t1' = max t1 lo
        t2' = min hi t2
        ndrop = round $ (t1' - t1)/dt
        ntake = round $ (t2' - t1')/dt
    in Signal t1' t2' dt (SV.take ntake $ SV.drop ndrop arr) eqOrK


limitSig' lo hi (Signal t1 t2 dt arr eq) 
    | t1 > lo || t2< hi = Nothing
    | otherwise =     let t1' = max t1 lo
                          t2' = min hi t2
                          ndrop = round $ (t1' - t1)/dt
                          ntake = round $ (t2' - t1')/dt
                      in Just $ Signal t1' t2' dt (SV.take ntake $ SV.drop ndrop arr) eq

copySig :: Signal a -> Signal a
copySig (Signal t1 t2 dt arr eq) = (Signal t1 t2 dt (SV.copy arr) eq)


--can't use eqToK here
crossSigUp :: Ord a => a -> Signal a -> [Double]
crossSigUp thr s@(Signal t1 t2 dt arr Eq) = 
    idxsToTimes s $ SV.findIndices id $ SV.zipWith (f) arr (SV.tail arr)
        where f y1 y2 = y2 > thr && y1 < thr
crossSigUp thr s@(Signal t1 t2 dt arr (Kont k)) = 
    idxsToTimes s $ SV.findIndices id $ SV.zipWith (f) arr (SV.tail arr)
        where f y1 y2 = (k y2) > thr && (k y1) < thr

(??) :: (a->Bool) -> Signal a -> [(Double,())]
p ?? s@(Signal t1 t2 dt arr Eq) = 
    zip (idxsToTimes s $ SV.findIndices id $ SV.zipWith (p') arr (SV.tail arr)) (repeat ())
        where p' y1 y2 = p y2  && not (p y1)
p ?? s@(Signal t1 t2 dt arr (Kont k)) = 
    zip (idxsToTimes s $ SV.findIndices id $ SV.zipWith (p') arr (SV.tail arr)) (repeat ())
        where p' y1 y2 = p (k y2)  && not (p (k y1))



idxsToTimes :: Signal a -> [Int] -> [Double]
idxsToTimes (Signal t1 t2 dt _ _) = map $ (+t1) . (*dt) . realToFrac
                    

combineSigs op s1@(Signal t1 t2 dt _ eok) s2@(Signal t1' t2' dt' _ eok')  = -- | dt == dt'
    let t1f = max t1 t1'
        t2f = min t2 t2'
        arr1 = sigArr $ forceSigEq $ limitSig t1f t2f s1
        arr2 = sigArr $ forceSigEq $ limitSig t1f t2f s2
        narr = SV.zipWith op arr1 arr2
    in Signal t1f t2f dt' narr Eq
combineSigs op (ConstSig x) (Signal t1 t2 dt arr eqOrK)  = -- | dt == dt'
    Signal t1 t2 dt arr $ Kont $ (x `op`) . eqOrKToF eqOrK
combineSigs op s1@(Signal t1 t2 dt sf _) s2@(ConstSig x) = combineSigs (flip op) s2 s1
combineSigs op (ConstSig x) (ConstSig y) = ConstSig $ op x y


onlyPos x | x > 0 = x
          | otherwise = 0

{-addLondAndShortSig :: (Storable a, Num a) => Signal a -> Signal a -> Signal a
addLondAndShortSig long@(Signal t1 t2 dt arr Eq) short@(Signal t1' t2' arr' Eq) =
    let beforeN = onlyPos $ (t1' - t1)/dt 
        afterN = onlyPos $ (t2 - t2')/dt 
        middleN = (max t1 t1' - min t2 t2')/dt
        ndropShort = -}

instance (Storable a, Eq a) => Eq (Signal a) where
    (Signal t1 t2 dt arr Eq) == (Signal t1' t2' dt' arr' Eq) = 
        t1 == t1' && t2 == t2' && dt == dt' && arr==arr'
    (ConstSig x) == (ConstSig y) = x==y
    s1@(Signal _ _ _ _ _) == s2@(Signal _ _ _ _ _) = forceSigEq s1 == forceSigEq s2
    _ == _ = False

instance (Storable a, Num a) => Num (Signal a) where
     (+) = combineSigs (+)
     (-) = combineSigs (-)
     (*) = combineSigs (*)
     abs s = abs `fmap` s
     signum s = undefined
     fromInteger x = ConstSig . fromInteger $ x

instance (Storable a, Fractional a) => Fractional (Signal a) where
    (/) = combineSigs (/)
    fromRational x =  ConstSig . fromRational $ x

instance (Storable a, Floating a) => Floating (Signal a) where
    pi = ConstSig pi
    cos = fmap cos
    sin = fmap sin
    tan = fmap tan
    log = fmap log
    exp = fmap exp
    acos = fmap acos
    asin = fmap asin
    atan = fmap atan
    acosh = fmap acosh
    asinh = fmap asinh
    atanh = fmap atanh
    cosh = fmap cosh
    sinh = fmap sinh
    sqrt = fmap sqrt

instance (Storable a, Reify a, Show a) => Reify (Signal a) where
    reify s@(SigV t1 t2 dt sf) = let arr = SV.pack $ map (unsafeReify . sf) [0..sigVPnts s-1]
                                 in Just $ Signal t1 t2 dt arr Eq -- $ \ix-> unsafeReify (sf ix)
    pack sig@(Signal t1 t2 dt sf eok) = SigV t1 t2 dt $ \ix -> pack (readSigPt sig ix) 
    pack s = error $ show s
    typeOfReified s = SignalT (typeOfReified (unSig s))
        where unSig :: Signal a -> a
              unSig = undefined

       
svInterpCos :: Int -> SV.Vector Double -> SV.Vector Double
svInterpCos n arr = let dstep = 1/(realToFrac n)
                        steps = map ((*dstep) . realToFrac) [0..(n-1)]
                        f (y1,y2) = let mu2s = map (\mu->(1-cos(mu*pi))/2) steps 
                                    in  map (\mu2-> (y1*(1-mu2)+y2*mu2)) mu2s
                    in SV.pack $ concatMap f $ SV.zip arr $ SV.tail arr

myZip = SV.zipWith (,)


svInterpLin :: Int -> SV.Vector Double -> SV.Vector Double
svInterpLin n arr = let dstep = 1/(realToFrac n)
                        steps = SV.pack $ map ((*dstep) . realToFrac) [0..(n-1)]
                        f (y1,y2) = SV.map (\mu-> (y1*(1-mu)+y2*mu)) steps
                    in SV.concatMap f $ myZip arr $ SV.tail arr

svInterpLinOffset :: Double -> SV.Vector Double -> SV.Vector Double
svInterpLinOffset mu arr = let f y1 y2 =  (y1*(1-mu)+y2*mu)
                           in SV.zipWith f arr (SV.tail arr)

svInterpLinST :: Int -> SV.Vector Double -> SV.Vector Double
svInterpLinST n arr = let dstep = 1/(realToFrac n)
                          --steps = map ((*dstep) . realToFrac) [0..(n-1)]
                          --f (y1,y2) = SV.map (\mu-> (y1*(1-mu)+y2*mu)) steps
                          oldLen = SV.length arr
                      in runST $ do
                        narr <- SVST.new_ ((oldLen-1) * n) 
                        forM_ [0..oldLen-2] $ \oi -> do
                                   let y1 = arr `SV.index` oi
                                   let y2 = arr `SV.index` (oi+1)
                                   forM_ [0..(n-1)] $ \mui -> do
                                                       let mu = (*dstep) . realToFrac $ mui
                                                       SVST.write narr (oi*n+mui) (y1*(1-mu)+y2*mu)
                        (SVST.freeze narr)


--return(y1*(1-mu)+y2*mu)
unjitterSig :: Signal Double -> Signal Double
unjitterSig (Signal t1 t2 dt arr Eq) =  let off = (roundToFrac dt t1) - t1
                                            narr = svInterpLinOffset (off/dt) arr
                                        in Signal (t1+off) (t2+off-dt) dt narr Eq
unjitterSig s = unjitterSig $ forceSigEq s

 

svInterpCubic :: Int -> SV.Vector Double -> SV.Vector Double
svInterpCubic n arr = let dstep = 1/(realToFrac n)
                          mus = map ((*dstep) . realToFrac) [0..(n-1)]
                          lst = SV.unpack arr
                          zps = zip4 (lst) (tail lst) (tail $ tail lst) (tail $ tail $ tail lst)
                          f (y0,y1, y2, y3) = let mu2s = map (\mu->mu*mu) mus
                                                  a0 = y3 - y2 - y0 + y1
                                                  a1 = y0 - y1 - a0
                                                  a2 = y2 - y0
                                                  a3 = y1
                                      in  map (\(mu,mu2)-> a0*mu*mu2+a1*mu2+a2*mu+a3) $ zip mus mu2s
                      in SV.pack $ concatMap f $ zps


listToSig dt t1 lst = let arr = SV.pack lst
                          t2 = (realToFrac $ SV.length arr-1) *dt +t1
                      in Signal t1 t2 dt arr Eq

sineSig :: Signal Double
sineSig = Signal 0 3 (0.1) (SV.pack $ map sin [0,0.1..3]) Eq

sigRevArr (Signal t1 t2 dt arr eq) = (Signal t1 t2 dt (SV.reverse arr) eq)

showItx nm = showItx' nm . forceSigEq

showItx' :: String -> Signal Double -> String
showItx' nm (Signal t1 t2 dt arr Eq) = 
    unlines $ ["IGOR", "WAVES\t"++nm, "BEGIN"]++
              (map show $ SV.unpack arr)++
              ["END", 
               "X SetScale/P x "++^t1++","++^dt++",\"\", "++nm++";"]
    where x++^y =x++(show y)

--http://local.wasp.uwa.edu.au/~pbourke/miscellaneous/interpolation/
--mu2 = (1-cos(mu*PI))/2;
--return(y1*(1-mu2)+y2*mu2);

{-
   mu2 = mu*mu;
   a0 = y3 - y2 - y0 + y1;
   a1 = y0 - y1 - a0;
   a2 = y2 - y0;
   a3 = y1;

   return(a0*mu*mu2+a1*mu2+a2*mu+a3);
-}

fillSig :: Storable a => Double -> Double -> Double -> (Double -> a) -> Signal a
fillSig t1 t2 dt f = let vls = map f $ timePointsFromT1T2Dt t1 t2 dt
                     in Signal t1 t2 dt (SV.pack vls) Eq

fillSigOver :: Storable a => [((Double,Double), b)] -> Double -> (b-> Double -> a) -> [Signal a]
fillSigOver durs dt f = map g durs
    where g ((t1,t2),v) = fillSig t1 t2 dt (\t -> f v (t-t1))