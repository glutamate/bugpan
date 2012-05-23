{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Format2 where

import EvalM
import Data.Binary 
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Numbers
--import Array
import Control.Monad 
import TNUtils
import Debug.Trace
import Unsafe.Coerce
import Data.Array.IO
import System.IO
import Data.Array.MArray
import qualified Data.StorableVector as SV
import Foreign.C.Types
import ValueIO

typeTag :: T -> [Word8]
typeTag BoolT = [1]
typeTag (NumT (Just IntT)) = [2]
typeTag (NumT (Just RealT)) = [3]
typeTag (NumT (Just CmplxT)) = [4]
typeTag UnitT = [5]
typeTag (PairT t1 t2) = [6] -- ++ typeTag t1 ++ typeTag t2
typeTag (ListT t) = [7] -- ++ typeTag t 
typeTag (SignalT t) = [8]
typeTag (StringT) = [9]

putTT :: V -> Put 
putTT v = mapM_ putWord8 . typeTag . typeOfVal $ v

data OV  =   OBoolV Bool
           | ONumV ONumVl
           | OPairV OV OV
           | OListV [OV]
	   | OLamV (OV->EvalM OV)
	   | OSigV Double Double Double (Int->OV)
           | OBoxV OV OV OV --shape,loc,  colour
           | OUnit
           | OStringV String
	     deriving (Show, Read)

data ONumVl = OldInt Int
              | OldReal Double
                deriving (Show, Read)

oldVtoV (OBoolV b) = BoolV b
oldVtoV (ONumV (OldInt i))= NumV $ NInt i
oldVtoV (ONumV (OldReal i))= NumV . NReal $ i
oldVtoV (OPairV p1 p2) = PairV (oldVtoV p1) (oldVtoV p2)
oldVtoV (OListV vs) = ListV (map oldVtoV vs)
oldVtoV (OUnit) = Unit
oldVtoV (OSigV t1 t2 dt sf) = SigV ( t1) ( t2) ( dt) (oldVtoV . sf)
oldVtoV (OStringV s ) = StringV s



instance Binary OV where
    put = undefined
    get = getFull


putFull v@(BoolV b) = putTT v >> put b
putFull v@(NumV (NInt i)) =  putTT v >>put i
putFull v@(NumV (NReal r)) =  putTT v >>put r
putFull v@(PairV v1 w1) = putTT v >> putFull v1 >> putFull w1
putFull v@(ListV xs) = putTT v >> put (length xs) >> mapM_ putFull xs
putFull v@(SigV t1 t2 dt sf) = do putTT v 
                                  put t1 
                                  put t2
                                  put dt
                                  mapM_ (\t->putFull $ sf t) [0..round $ (t2-t1)/dt]
putFull Unit = putTT Unit
putFull v@(StringV s) = putTT v >> put s


getFull = do tt1 <- get
             case idWord8 tt1 of
               1 -> OBoolV `fmap` get 
               2 -> (ONumV . OldInt) `fmap` get 
               3 -> (ONumV . OldReal) `fmap` get 
               5 -> do () <- get
                       return OUnit
               6 -> do p1 <- getFull
                       p2 <- getFull
                       return $ OPairV p1 p2
               7 -> do n <- idInt `fmap` get
                       --vls <- get
                       vls <- forM [0..n-1] $ const getFull
                       return $ OListV vls
{-               8 -> do t1 <- get
                       t2 <- get
                       dt <- get
                       vls <- forM [t1,t1+dt..t2] $ const getFull
                       let arr = listArray (0, length vls -1) vls
                       return . OSigV t1 t2 dt $ \pt->arr!pt -}
               9 -> OStringV `fmap` get 
               tt -> error $ "unknown type tag: "++show tt
    where idWord8 :: Word8 -> Word8
          idWord8 = id


getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get 
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}


--- SPECIALIZE getMany :: Int -> Get [RealNum] -} 






