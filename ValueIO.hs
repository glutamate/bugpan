{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module ValueIO where

import EvalM
import Data.Binary 
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Numbers
--import Array
import Control.Monad 
import TNUtils
import Debug.Trace
import Unsafe.Coerce
--import Data.Array.IO
import System.IO
--import Data.Array.MArray
--import Data.Array.Vector
import qualified Data.StorableVector as SV
import Foreign.C.Types
import Data.Binary.IEEE754
import Data.Int
import Data.List
import Numeric
import PrettyPrint
import System.Cmd

loadVs :: String -> IO [V]
loadVs fp = loadBinary fp

saveVs :: String -> [V] -> IO ()
saveVs fp sigs@((SigV _ _ _ _):_) | typeOfVal (head sigs) == SignalT (NumT (Just RealT)) = do
  let dir = "/"++(intercalate "/" $ init $ splitBy '/' fp)++"/"
  forM_ (zip sigs [1..]) $ \(sig,n) -> do
    let nm = if length sigs ==1
                then fp
                else (dir++(showHex n ""))
    h <- openBinaryFile nm WriteMode
    L.hPut h $ encode (length [sig])
    hWriteSigV h sig
    hClose h 
                                  | otherwise = saveBinary fp sigs
saveVs fp obj = saveBinary fp obj 

binPut :: MyBinary a => Handle -> a -> IO ()
binPut h x = L.hPut h (runPut $ myPut x)

binGet :: MyBinary a => Handle -> Int -> IO a
binGet h n = (runGet myGet) `fmap` L.hGet h n 

hWriteSigV h s@ (SigV t1 t2 dt sf) = do
  binPut h $ typeTag1 . typeOfVal $ s
  binPut h t1
  binPut h t2
  binPut h dt
--  L.hPut h (runPut $ putWord32le ((round $ (t2-t1)/dt)::Word32))
  SV.hPut h $ SV.pack $ map (idDouble . unsafeReify . sf) $ [0..(round $ (t2-t1)/dt)-1]


hWriteSigReal :: Handle -> Signal Double -> IO ()
hWriteSigReal h s@(Signal t1 t2 dt sf) = do
  binPut h $ ([8,3]::[Word8])
  binPut h t1
  binPut h t2
  binPut h dt
--  L.hPut h (runPut $ putWord32le ((round $ (t2-t1)/dt)::Word32))
  SV.hPut h $ SV.pack $ map  sf $ [0..(round $ (t2-t1)/dt)-1]

writeSigReal :: String -> Signal Double -> IO ()
writeSigReal fnm sig = do
   h <- openBinaryFile fnm WriteMode
   L.hPut h $ encode (length [sig])
   hWriteSigReal h sig
   hClose h 
 
typeTag1 :: T -> [Word8]
typeTag1 BoolT = [1]
typeTag1 (NumT (Just IntT)) = [2]
typeTag1 (NumT (Just RealT)) = [3]
typeTag1 (NumT (Just CmplxT)) = [4]
typeTag1 UnitT = [5]
typeTag1 (PairT t1 t2) = [6] ++ typeTag1 t1 ++ typeTag1 t2
typeTag1 (ListT t) = [7]  ++ typeTag1 t 
typeTag1 (SignalT t) = [8]++ typeTag1 t
typeTag1 (StringT) = [9]

parseTT :: [Word8] -> T
parseTT wds = fst . parseTT1 $ wds
 
parseTT1 :: [Word8] -> (T, [Word8])
parseTT1 (1:rest) = (BoolT ,rest)
parseTT1 (2:rest) = ((NumT (Just IntT)),rest)
parseTT1 (3:rest) = ((NumT (Just RealT)),rest)
parseTT1 (4:rest) = ((NumT (Just CmplxT)),rest)
parseTT1 (5:rest) = (UnitT, rest)
parseTT1 (9:rest) = (StringT, rest)
parseTT1 (7:rest) = let (t,more) = parseTT1 rest
                    in (ListT t, more)
parseTT1 (8:rest) = let (t,more) = parseTT1 rest
                    in (SignalT t, more)
parseTT1 (6:rest) = let (t1,more) = parseTT1 rest
                        (t2,more') = parseTT1 more
                    in (PairT t1 t2, more')
parseTT1 wds = error $ "parseTT1: unknonw type tag "++show wds

putTT1 :: V -> Put 
putTT1 v = put . typeTag1 . typeOfVal $ v

aSig = SigV 0 1 0.1 $ \p -> NumV ((realToFrac p) / 100)
aSigT = Signal 0 1 0.1 $ \p -> ((realToFrac p) / 100)

instance Binary V where
    put v = putTT1 v >> putRaw v
    --put = putFull
    get = (parseTT `fmap` get) >>= getRaw -- getFull
    --get = getFull

putRaw v@(BoolV b) =  put b
putRaw v@(NumV (NInt i)) =  put i
putRaw v@(NumV (NReal r)) =  putD r
putRaw v@(PairV v1 w1) =  putRaw v1 >> putRaw w1
putRaw v@(ListV xs) =  put (length xs) >> mapM_ putRaw xs
putRaw v@(SigV t1 t2 dt sf) = error $ "putRaw signal! "++show v++" :: "++ppType (typeOfVal v) {-do 
  putD $ t1 
  putD $ t2
  putD $ dt
--  putWord32le ((round $ (t2-t1)/dt)::Word32)
  mapM_ (\t->putRaw $ sf t) [0..round $ (t2-t1)/dt] -}
putRaw Unit = put ()
putRaw v@(StringV s) = put s

binTest x = let y = decode $ encode x
            in if y==x 
                  then Nothing
                  else Just (x,y)

binShow x = let y = decode $ encode x
            in show (y `asTypeOf` x)

sig0 (SigV _ _ _ sf) = sf 0

getRaw BoolT = BoolV `fmap` get 
getRaw (NumT (Just RealT)) = (NumV . NReal) `fmap` getD 
getRaw (NumT (Just IntT)) = (NumV . NInt) `fmap` get 
getRaw (UnitT) = return Unit
getRaw (StringT) = StringV `fmap` get
getRaw (PairT t1 t2) = do v1 <- getRaw t1
                          v2 <- getRaw t2
                          return $ PairV v1 v2
getRaw (ListT t) = do n <- get :: Get Int
                      ListV `fmap` getManyRaw n t
{-getRaw (SignalT (NumT (Just RealT))) = do 
                        t1 <- get
                        t2 <- get
                        dt <- get 
                        let n = round $ (t2-t1)/dt
                        vls <- getMany n 
                        let arr = listArray (0, length vls -1) vls
                        return . SigV t1 t2 dt $ \pt->arr!pt 
getRaw (SignalT t) = do t1 <- getD
                        t2 <- getD
                        dt <- getD
                        --n <- fmap (fromInteger . toInteger) getWord32le
                        let n = round $ (t2-t1)/dt
                        vls <- getManyRaw n t
                        let arr = listArray (0, length vls -1) vls
                        return . SigV t1 t2 dt $ \pt->arr!pt
                       -} 

-- copied from Data.Binary
getRaw t = error $ "getRaw unknown type: "++show t
getManyRaw :: Int -> T -> Get [V]
getManyRaw n t = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- getRaw t
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getManyRaw #-}

getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get 
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

myGetMany :: MyBinary a => Int -> Get [a]
myGetMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- myGet 
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE myGetMany #-}


{-# INLINE getRaw #-}

{- --SPECIALIZE getMany :: Int -> Get [RealNum] -} 


idWord8 :: Word8 -> Word8
idWord8 = id

sigVat (SigV t1 t2 dt sf) t = sf $ round $ (t-t1)/dt

class Binary a => MyBinary a where
    myGet :: Get a
    myGet = get

    myPut :: a-> Put
    myPut = put
    

instance MyBinary Double where
    myGet = getD
    myPut = putD

instance Binary (Signal Double) where
    get = do t1 <- getD
             t2 <- getD
             dt <- getD
             n <- fmap (fromInteger . toInteger) getWord32le
             vls <- myGetMany n
             let arr = SV.pack vls
             return . Signal t1 t2 dt $ \pt->arr `SV.index` pt
    put (Signal t1 t2 dt sf)= do putD t1 
                                 putD t2
                                 putD dt
                                 --putWord32le ((round $ (t2-t1)/dt)::Word32)
                                 mapM_ (\t->putD $ sf t) [0..round $ (t2-t1)/dt]

instance MyBinary (Signal Double)
instance MyBinary Bool
instance MyBinary ()
instance MyBinary Int
instance MyBinary Word8
instance (MyBinary a, MyBinary b) => MyBinary (a,b) where
    myGet = return (,) `ap` myGet `ap` myGet 
    myPut (x,y) = myPut x >> myPut y

instance (MyBinary a) => MyBinary [a] where
    myPut xs= put (length xs) >> forM_ xs myPut
    myGet = get >>= myGetMany
    
loadSignalsU:: String -> IO [Signal Double]
loadSignalsU fp = do
  let expectedTypeTag = SignalT $ NumT $ Just RealT
  h <- openBinaryFile fp ReadMode
  n <- idInt `fmap` binGet h 8
  forM [1..n] (\i -> loadOneSigSV h)

loadOneSigSV :: Handle -> IO (Signal Double)
loadOneSigSV h = do
  ntytag <- idInt `fmap` binGet h 8
  hSeek h RelativeSeek (-8)
  tytag <- (fst . parseTT1) `fmap` binGet h (ntytag+8)
  t1 <- binGet h 8
  t2 <- binGet h 8
  dt <- binGet h 8
  --print (t1,t2,dt)
  arr <- SV.hGet h (round $ (t2-t1)/dt)
  return $ Signal t1 t2 dt $ \p-> arr `SV.index` p

readN'TT h = do 
      n <- idInt `fmap` binGet h 8
      ntytag <- idInt `fmap` binGet h 8
      hSeek h RelativeSeek (-8)
      tytag <- (fst . parseTT1) `fmap` binGet h (ntytag+8)
      return (n,tytag)

class LoadDirectly a where
    loadDirectly :: String -> IO a
 
instance LoadDirectly [(Double,())] where
    loadDirectly fp = do
      h <- openBinaryFile fp ReadMode
      (n,tytag) <- readN'TT h
      print2 fp (n,tytag)
      arr <- SV.hGet h n
      return $ zip (SV.unpack arr) $ repeat ()

instance LoadDirectly [(Double,Double)] where
    loadDirectly fp = do
      h <- openBinaryFile fp ReadMode
      (n,tytag) <- readN'TT h
      print2 fp (n,tytag)
      arr <- SV.hGet h (n*2)
      return $ listToListOfPairs (SV.unpack arr)

instance LoadDirectly [((Double,Double),())] where
    loadDirectly fp = do
      h <- openBinaryFile fp ReadMode
      (n,tytag) <- readN'TT h
      print2 fp (n,tytag)
      arr <- SV.hGet h (n*2)
      return $ zip (listToListOfPairs (SV.unpack arr)) $ repeat ()

instance LoadDirectly [((Double,Double),Double)] where
    loadDirectly fp = do
      h <- openBinaryFile fp ReadMode
      (n,tytag) <- readN'TT h
      print2 fp (n,tytag)
      arr <- SV.hGet h (n*3)
      return $ (funny (SV.unpack arr))
          where funny (x:y:z:rest) = ((x,y),z) : funny rest
                funny _ = []


listToListOfPairs :: [a] -> [(a,a)]
listToListOfPairs (x:y:rest) = (x,y) : listToListOfPairs rest
listToListOfPairs _ = []

--getArr h n= SV.hGet h n

testLSU = do
  saveVs "aSig" [aSig, aSig]
  sigs <- loadSignalsU "aSig"
  print2 "#sigs " $ length sigs
  print2 "head sigs " $ head sigs
  print2 "2nd sig " $ sigs!!1
  
  return ()


loadReifiedBinary :: (Reify a,MyBinary a, Show a) => String -> IO [a]
loadReifiedBinary fp = res where
    res = do let expectedTypeTag = typeOfReified (head $ unIO res)
             bs <- L.readFile fp
             let (n, bs1,_) =  runGetState (get) bs 1000
             --return $ idInt n
             let (actualTypeTag, restOfFile, _)  =runGetState (parseTT `fmap` get) bs1 1000
             when (actualTypeTag /= expectedTypeTag) (fail $ "laodReifyBin "++fp++": "++show expectedTypeTag ++ " != "++ show actualTypeTag)
             --putStrLn $ show expectedTypeTag ++ " =?= "++ show actualTypeTag
             let objs =runGet (forM [1..(idInt n)] $ const myGet) restOfFile
             --let obj =runGet (get) restOfFile 
             --print objs
             return $ objs

fileTypeTag :: String -> IO T
fileTypeTag fp = do bs <- L.readFile fp
                    let (n, bs1,_) =  runGetState (get) bs 1000
                    let (actualTypeTag, restOfFile, _)  =runGetState (parseTT `fmap` get) bs1 1000
                    let n'= idInt n
                    return $ actualTypeTag
             --return $ idInt n

unIO :: IO a -> a
unIO = undefined

test1 n = do let x = 2.0::RealNum
             saveBinary ("/var/bugpan/testBin"++show n) $ replicate n x

getD = getFloat64le
putD = putFloat64le

{-instance Binary RealNum where
    put (RealNum x) =  putFloat64le x
    get = RealNum  `fmap` getFloat64le  -}

-- getFloat64le
toWord64 :: a -> Word64
toWord64 = unsafeCoerce

fromWord64 :: Word64 -> a
fromWord64 = unsafeCoerce



idDouble :: Double -> Double
idDouble = id

idDoubleL :: [Double] -> [Double]
idDoubleL = id




