{-# LANGUAGE Rank2Types #-}

module EvalM where

import Control.Monad
import Data.Maybe
--import Control.Monad.Reader
--import Control.Monad.Error
--import Control.Monad.Identity
--import System.IO.Unsafe

import Numbers

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
        | ULamV (forall a. V->(V->a)->a)
	| SigV Double Double (Double->V)
	| USigV (forall a. Double->(V->a)->a)
        | BoxV V V V --shape,loc,  colour
        | Unit

data T  = BoolT
	| NumT
	| PairT T T
	| LamT T T
	| ListT T
	| AnyT
	| StringT
	| SignalT T
	| EventT T
	| EpochT T
        | ShapeT 
	deriving (Show, Eq)

instance Eq V where
    BoolV x == BoolV y = x==y
    NumV x == NumV y = x==y
    PairV x w == PairV y z = x==y && w==z
    Unit == Unit = True
    _ == _ = False

instance Show V where
    show (BoolV True) = "True"
    show (BoolV False) = "False"
    show (NumV v) = show v
    show (LamV _) = "<lambda value>"
    show (SigV t1 t2 _) = "<signal value "++show t1++" to  "++show t2++">"
    show (PairV (PairV x y) z) = "("++show x++","++show y++","++show z++")"
    show (PairV v w) = "("++show v++","++show w++")"
    show (Unit) = "()"
    show (ListV vs) =  "["++slist vs++"]"
        where slist [] = ""
              slist (x:[]) = show x
              slist (x:xs) = show x ++ ", " ++ slist xs
    show (BoxV shp loc col) = "Box " ++show shp++" @"++show loc++" RGB"++show col



withTime t  es =  es {cur_t=Just t}

withoutTime  es = es {cur_t= Nothing }

extEnv :: (String,V) -> EvalS -> EvalS
extEnv p@(n,v) es = es {env=(p:env es)}

extsEnv :: [(String,V)] -> EvalS -> EvalS
extsEnv ps es = es {env=(ps++env es)}

type Env = [(String, V)]

instance Show (a->b) where
    show _ = "<fun>"

instance Eq (a->b) where
    _ == _ = undefined

boolToV = BoolV

vToBool (BoolV b)= return b
vToBool _ = fail "non-boolean predicate"

vToDbl (NumV n) = return $ numToDouble n
vToDbl (e) = error $ "expected nuymber, got "++show e

unsafeVToDbl (NumV n) =  numToDouble n

unLamV (LamV f) = return f
unLamV _ = fail "expected function argument"

unListV (ListV vs) = return vs
unListV _ = fail "expected list argument"

unPairV (PairV x y)= return $ (x,y)
unPairV pr = fail $ "expected PairV, got "++show pr

fromMaybe (Just x) _ = return x
fromMaybe Nothing errs = fail errs

guardF True s = return ()
guardF False s = fail s


pair3 x y z = (PairV (PairV x y) z)
