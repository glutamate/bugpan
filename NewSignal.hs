{-# LANGUAGE GADTs #-}

module NewSignal where

--import 
import qualified Data.StorableVector as SV
import Foreign.Storable

data EqOrK a b where
    Eq :: EqOrK a a
    Kont :: (a->b) -> EqOrK a b

eqOrKToF :: EqOrK a b -> (a->b)
eqOrKToF (Eq) = id
eqOrKToF (Kont f) = f

data Signal a where
    Signal :: Storable a => Double -> Double -> Double -> SV.Vector a -> EqOrK a b -> Signal b
    ConstSig :: a -> Signal a

sigArr (Signal t1 t2 dt arr eqOrK ) = SV.map (eqOrKToF eqOrK) arr

forceSigEq :: (Storable a) => Signal a -> Signal a 
forceSigEq s@(ConstSig x) = s
forceSigEq s@(Signal _ _ _ _ Eq) = s
forceSigEq (Signal t1 t2 dt arr (Kont f)) =let arr' = SV.map f arr
                                           in Signal t1 t2 dt arr' Eq

instance Functor Signal where
    fmap f (ConstSig x) = ConstSig $ f x
    fmap f (Signal t1 t2 dt arr Eq) = Signal t1 t2 dt arr $ Kont f
    fmap f (Signal t1 t2 dt arr (Kont g)) = Signal t1 t2 dt arr $ Kont $ f . g

sigPnts :: Signal a -> Int
sigPnts (Signal t1 t2 dt sf _ ) = round $ (t2-t1)/dt

sigToList :: Signal a -> [a]
sigToList sig@(Signal t1 t2 dt arr Eq) = map (arr `SV.index`) [0..sigPnts sig-1]
sigToList sig@(Signal t1 t2 dt arr (Kont f)) = map (f . (arr `SV.index`)) [0..sigPnts sig-1]

instance Show a =>  Show (Signal a) where
    show sig@(Signal t1 t2 dt arr _) = 
        "{"++show t1++": "++(show . take 5 $ sigToList sig)++"... :"++show t2++"}"
    show (ConstSig x) = "ConstSig "++show x

readSig :: Signal a -> Double -> a
readSig (Signal t1 t2 dt arr eqOrK)  t = 
    (eqOrKToF eqOrK) $ arr `SV.index` (round $ (t-t1 )/dt)

limitSig lo hi (Signal t1 t2 dt arr eqOrK) = 
    let t1' = max t1 lo
        t2' = min hi t2
        ndrop = round $ (t1' - t1)/dt
        ntake = round $ (t2' - t1')/dt
    in Signal t1' t2' dt (SV.take ntake $ SV.drop ndrop arr) eqOrK

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
       
svInterpCos :: Int -> SV.Vector Double -> SV.Vector Double
svInterpCos n arr = let dstep = 1/(realToFrac n)
                        steps = map ((*dstep) . realToFrac) [0..n]
                        f (y1,y2) = let mu2s = map (\mu->(1-cos(mu*pi))/2) steps 
                                    in  map (\mu2-> (y1*(1-mu2)+y2*mu2)) mu2s
                    in SV.pack $ concatMap f $ SV.zip arr $ SV.tail arr
 

--http://local.wasp.uwa.edu.au/~pbourke/miscellaneous/interpolation/
--mu2 = (1-cos(mu*PI))/2;
--return(y1*(1-mu2)+y2*mu2);