{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Numbers where

import Data.Complex
import Text.Printf
import Data.Generics
--import Data.Array.Vector
type Number = NumVl

--newtype RealNum = RealNum { unRealNum :: Double} 
--    deriving (Eq, Enum, Read, Num, Ord, Real, Fractional, RealFloat, RealFrac, Floating)
type RealNum = Double

--instance Show RealNum where
--    show (RealNum x) = show x

instance Bounded Double where
    minBound = -1e200
    maxBound = 1e200

--real = RealNum 1.0



data NumVl = NInt Int
	   -- | NRat Int Int
	   | NReal RealNum
	   | NCmplx (Complex RealNum)
	     deriving (Show, Read, Data, Typeable)

instance Eq NumVl where
    n1 == n2 = let (x,y) = sameize n1 n2 in x =~= y
    
(NInt n1) =~= (NInt n2) = n1==n2
(NReal n1) =~= (NReal n2) | nearlyZero n1 && nearlyZero n2 = True
                          | otherwise = abs (n1-n2)/(max (abs n1) (abs n2)) <0.0000001

nearlyZero n = (n<1e-18) && (n>(-1e-18))

ppNum n = if n<0 then "("++(ppNum' n)++")" else ppNum' n
ppNum' (NInt i) = show i
ppNum' (NReal f) = printf "%5.4g" $ f
ppNum' (NCmplx (r:+c)) = show r++" + i*"++show c

data Ntype = NI | NR | NC deriving (Show,Eq, Enum, Ord)

num2numType (NInt _) = NI
num2numType (NReal _) = NR
num2numType (NCmplx _) = NC

numToDouble (NInt n) = realToFrac n
numToDouble (NReal d) = d
numToDouble (NCmplx c) = realPart c

numCast (NInt i) NR = NReal (fromIntegral i)
numCast (NInt i) NC = NCmplx $ fromIntegral i:+0
numCast (NReal r) NI = NInt $ round r
numCast (NReal r) NC = NCmplx $ r:+0
numCast (NCmplx c) NI = NInt . round $ realPart c
numCast (NCmplx c) NR = NReal $ realPart c
numCast (n) t = n

instance Ord NumVl where
	compare (NInt i1) (NInt i2) = compare i1 i2 
	compare (NReal i1) (NReal i2) = compare i1 i2 
	compare (NCmplx i1) (NCmplx i2) = compare (magnitude i1) (magnitude i2)
	compare n1 n2 = let (v1, v2) = sameize n1 n2 in compare v1 v2

tst =  negate (NReal 1.1)

instance Num NumVl where
	(+) = combine (+) 
	(-) = combine (-) 
	(*) = combine (*) 
	negate e = (NInt (-1))*e
	abs e = if e<0 then (negate e) else (e)
	signum e = if e<0 then (-1) else (1)
	fromInteger i = NInt (fromInteger i)

instance Fractional NumVl where
	(NInt n1) / (NInt n2) = NReal $ (realToFrac n1)/(realToFrac n2)
	(NReal n1) / (NReal n2) = NReal $ n1/n2
	(NCmplx n1) / (NCmplx n2) = NCmplx $ n1/n2
	n1 / n2 = let (v1, v2) = sameize n1 n2 in v1 / v2
	fromRational r = NReal $ fromRational r

instance Floating NumVl where
	cos = onDbl cos
	sin = onDbl sin
	tan = onDbl tan
	log = onDbl log
	exp = onDbl exp
	acos = onDbl acos
	asin = onDbl asin
	atan = onDbl atan
	acosh = onDbl acosh
	asinh = onDbl asinh
	atanh = onDbl atanh
	cosh = onDbl cosh
	sinh = onDbl sinh
        sqrt = onDbl sqrt
	pi = NReal pi
	

onDbl :: (RealNum->RealNum) -> NumVl -> NumVl
onDbl op x = NReal . op $ numToDouble x

sameize v1 v2 = let  	t1 = num2numType v1
			t2 = num2numType v2
			resTp = max t1 t2 in
		(numCast v1 resTp, numCast v2 resTp)

combine :: (forall a. Num a => a->a->a) -> NumVl -> NumVl -> NumVl
combine op v1 v2 = let  t1 = num2numType v1
			t2 = num2numType v2
			resTp = max t1 t2 in
		   combineSame op (numCast v1 resTp) (numCast v2 resTp)
			
combineSame :: (forall a. Num a => a->a->a) -> NumVl -> NumVl -> NumVl
combineSame op (NInt i1) (NInt i2) = NInt $ op i1 i2
combineSame op (NReal r1) (NReal r2) = NReal $ op r1 r2
combineSame op (NCmplx c1) (NCmplx c2) = NCmplx $ op c1 c2

re (NCmplx c) = NReal $ realPart c
re n = n

im (NCmplx c) = NReal $ imagPart c
im n = 0

applyRealFun1 :: (forall a. Floating a => a->a) -> NumVl -> NumVl
applyRealFun1 f (NInt i) = NReal . f $ fromIntegral i 
applyRealFun1 f (NReal r) = NReal . f $ r
applyRealFun1 f (NCmplx c) = NCmplx . f $ c

applyNumFun2 :: (forall a. Num a => a->a->a) -> NumVl -> NumVl-> NumVl
applyNumFun2 f (NInt i1) (NInt i2) = NInt $ f i1 i2
applyNumFun2 f (NReal i1) (NReal i2) = NReal $ f i1 i2
applyNumFun2 f (NCmplx i1) (NCmplx i2) = NCmplx $ f i1 i2
applyNumFun2 f n1 n2 = let (v1, v2) = sameize n1 n2 in applyNumFun2 f v1 v2


natlog = applyRealFun1 log
natexp = applyRealFun1 exp

roundNum :: NumVl -> NumVl
roundNum n = numCast n NI

floorNum :: NumVl -> NumVl
floorNum (NInt i ) = NInt i
floorNum (NReal r) = NInt $ floor r
