{-# LANGUAGE Rank2Types, FlexibleInstances, OverlappingInstances, DeriveDataTypeable #-}

module EvalM where

import Control.Monad
import Data.Maybe
import Data.Binary
--import Control.Monad.Reader
--import Control.Monad.Error
--import Control.Monad.Identity
--import System.IO.Unsafe
--import Debug.Trace
import Numbers
import Data.Array
import Data.Typeable

data EvalS = EvalS { dt:: Double,
                     tmax :: Double,
                     cur_t:: (Maybe Double),
                     env:: Env }

curTime = fromJust . cur_t

--type EvalM a = ErrorT String (ReaderT EvalS Identity) a

data EvalM a = Res a | Error String

emptyEvalS = EvalS 1 1 Nothing []
--    deriving (Functor, Monad, MonadPlus, Error, MonadReader)

instance Functor EvalM where
    fmap f (Res x) = Res $ f x
    fmap _ (Error s) = Error s

instance Monad EvalM where
    return x = Res x
    Res x >>= f = f x
    Error s >>= _ = Error s
    fail s = Error s

instance MonadPlus EvalM where
    mzero = Error "mzero"
    Res x `mplus` _ = Res x
    Error s `mplus` y = y

instance Show a => Show (EvalM a) where
    show (Res x) = show x
    show (Error s) = "error: "++s



{-unEvalM :: EvalS -> EvalM a -> a
unEvalM st era =   runIdentity $ runReaderT (do ra <- runErrorT  era 
                                                case ra of 
                                                      Right rra -> return rra
                                                      Left s -> fail s) st -}

unEvalM :: EvalM a -> a
unEvalM (Res x) = x
unEvalM (Error s) = error $ "unEvalM: "++s

runEvalM :: Monad m => EvalM a -> m a
runEvalM (Res x) = return  x
runEvalM (Error s) = fail s

sfEvalM :: EvalM a -> Either String a
sfEvalM (Res x) = Right x
sfEvalM (Error s) = Left s

data V  = BoolV Bool
        | NumV NumVl
        | PairV V V
        | ListV [V]
	| LamV (V->EvalM V)
	| SigV Double Double Double (Int->V)
        | BoxV V V V --shape,loc,  colour
        | Unit
        | StringV String
	deriving (Show, Read)

instance Eq V where
    BoolV x == BoolV y = x==y
    NumV x == NumV y = x==y
    PairV x w == PairV y z = x==y && w==z
    Unit == Unit = True
    StringV s1 == StringV s2 = s1 == s2  
    ListV v1s == ListV v2s = length v1s == length v2s && (and $ zipWith (==) v1s v2s)
    _ == _ = False



instance Read (a->b) 
instance Show (a->b) where
    show _ = "<fun>"

instance Eq (a->b) where
    _ == _ = undefined


data T  = BoolT
	| NumT (Maybe NumT)
	| PairT T T
	| LamT T T
	| ListT T
	| AnyT
	| SignalT T
	| EventT T
	| DurationT T
        | ShapeT 
        | UnitT
        | StringT
        | TyVar String
        | UnknownT String
        | UnspecifiedT
	deriving (Show, Eq, Read)

data NumT = IntT | RealT | CmplxT deriving (Eq, Show, Read, Ord)

withTime t  es =  es {cur_t=Just t}

withoutTime  es = es {cur_t= Nothing }

extEnv :: (String,V) -> EvalS -> EvalS
extEnv p@(n,v) es = es {env=(p:env es)}

extsEnv :: [(String,V)] -> EvalS -> EvalS
extsEnv ps es = es {env=(ps++env es)}

type Env = [(String, V)]

boolToV = BoolV

vToBool (BoolV b)= return b
vToBool _ = fail "non-boolean predicate"

vToDbl (NumV n) = return $ numToDouble n
vToDbl (e) = error $ "expected nuymber, got "++show e

unsafeVToDbl (NumV n) =  numToDouble n

unLamV (LamV f) = return f
unLamV _ = fail "expected function argument"

unListV (ListV vs) = return vs
unListV v = fail $ "expected list argument, got: "++show v

unPairV (PairV x y)= return $ (x,y)
unPairV pr = fail $ "expected PairV, got "++show pr

fromMaybe (Just x) _ = return x
fromMaybe Nothing errs = fail errs

guardF True s = return ()
guardF False s = fail s


pair3 x y z = (PairV (PairV x y) z)

typeOfVal :: V -> T
typeOfVal (BoolV _ ) = BoolT 
typeOfVal (NumV (NInt _)) = NumT $ Just IntT
typeOfVal (NumV (NReal _)) = NumT $ Just RealT
typeOfVal (NumV (NCmplx _)) = NumT $ Just CmplxT
typeOfVal (PairV v w) = PairT (typeOfVal v) (typeOfVal w)
typeOfVal (ListV []) = ListT AnyT
typeOfVal (ListV (x:_)) = ListT (typeOfVal x)
typeOfVal (SigV t1 _ _ sf) = SignalT (typeOfVal $ sf 0)
typeOfVal (BoxV _ _ _) = ShapeT
typeOfVal Unit = UnitT
typeOfVal (StringV _) = StringT


class Reify a where
    reify :: V-> Maybe a
    pack :: a->V

instance Reify V where 
    reify = Just
    pack = id
instance Reify Double where 
    reify = vToDbl
    pack = NumV . NReal
instance Reify Int where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just i
    reify _ = Nothing
    pack = NumV . NInt
instance Reify Integer where 
    reify (NumV n) = let NInt i = numCast n NI
                     in Just $ toInteger i
    reify _ = Nothing
    pack = NumV . NInt . fromInteger
instance (Reify a, Reify b) => Reify (a,b) where
    reify (PairV a b) = liftM2 (,) (reify a) (reify b)
    reify _ = Nothing
    pack (x,y) = PairV (pack x) (pack y)
instance (Reify a, Reify b, Reify c) => Reify (a,b,c) where
    reify (PairV (PairV a b) c) = liftM3 (,,) (reify a) (reify b) (reify c)
    reify _ = Nothing
    pack (x,y,z) = PairV (PairV (pack x) (pack y)) (pack z)
instance (Reify a) => Reify [a] where
    reify (ListV xs) = let rmxs = map reify xs in
                       if all isJust rmxs
                          then Just $ map fromJust rmxs
                          else Nothing
    reify _ = Nothing
    pack xs = ListV $ map pack xs
instance Reify NumVl where
    reify (NumV n) = Just n
    reify _ = Nothing
    pack = NumV
instance Reify () where
    reify Unit = Just ()
    reify _ = Nothing
    pack () = Unit

instance Reify [Char] where
    reify (StringV s) = Just s
    reify _ = Nothing
    pack str = StringV str

unsafeReify :: Reify a => V -> a
unsafeReify = fromJust . reify

--(Signal t1 t2 dt sf)
data Signal a = Signal Double Double Double (Int -> a) deriving Typeable


readSig :: Signal a -> Double -> a
(Signal t1 t2 dt sf) `readSig` t = sf . round $ (t-t1 )/dt

instance Reify a => Reify (Signal a) where
    reify (SigV t1 t2 dt sf) = Just $ Signal t1 t2 dt $ \ix-> unsafeReify (sf ix)
    pack (Signal t1 t2 dt sf) = SigV t1 t2 dt $ \ix->pack (sf ix)

