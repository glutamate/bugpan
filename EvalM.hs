{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
        | StrV String
        | PairV V V
        | ListV [V]
	| LamV (V->EvalM V)
	| SigV (Double->V)
        | Unit
          deriving (Show, Eq)

withTime t  es =  es {cur_t=Just t}

withoutTime  es = es {cur_t= Nothing }

extEnv :: (String,V) -> EvalS -> EvalS
extEnv p@(n,v) es = es {env=(p:env es)}

type Env = [(String, V)]

instance Show (a->b) where
    show _ = "<fun>"

instance Eq (a->b) where
    _ == _ = undefined

boolToV = BoolV

vToBool (BoolV b)= return b
vToBool _ = fail "non-boolean predicate"

unLamV (LamV f) = return f
unLamV _ = fail "expected function argument"

unListV (ListV vs) = return vs
unListV _ = fail "expected list argument"


fromMaybe (Just x) _ = return x
fromMaybe Nothing errs = fail errs

guardF True s = return ()
guardF False s = fail s
