module ValueIO where

import EvalM
import Data.Binary 
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Numbers
import Array
import Control.Monad 
import TNUtils
import Debug.Trace

loadVs :: String -> IO [V]
loadVs fp = loadBinary fp

saveVs :: String -> [V] -> IO ()
saveVs fp obj = saveBinary fp obj 

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

putTT :: V -> Put 
putTT v = mapM_ putWord8 . typeTag . typeOfVal $ v

putTT1 :: V -> Put 
putTT1 v = put . typeTag1 . typeOfVal $ v


newtype OldFmtV = OldFmtV { unOldFmtV :: V } deriving (Show, Eq)

instance Binary OldFmtV where
    put = putFull . unOldFmtV
    get = OldFmtV `fmap` getFull

instance Binary V where
    put v = putTT1 v >> putRaw v
    --put = putFull
    get = (parseTT `fmap` get) >>= getRaw -- getFull
    --get = getFull

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

putRaw v@(BoolV b) =  put b
putRaw v@(NumV (NInt i)) =  put i
putRaw v@(NumV (NReal r)) =  put r
putRaw v@(PairV v1 w1) =  putRaw v1 >> putRaw w1
putRaw v@(ListV xs) =  put (length xs) >> mapM_ putRaw xs
putRaw v@(SigV t1 t2 dt sf) = do put t1 
                                 put t2
                                 put dt
                                 mapM_ (\t->putRaw $ sf t) [0..round $ (t2-t1)/dt]
putRaw Unit = put ()
putRaw v@(StringV s) = put s

binTest x = let y = decode $ encode x
            in if y==x 
                  then Nothing
                  else Just (x,y)

binShow x = let y = decode $ encode x
            in show (y `asTypeOf` x)

sig0 (SigV _ _ _ sf) = sf 0

getFull = do tt1 <- get
             case idWord8 tt1 of
               1 -> BoolV `fmap` get 
               2 -> (NumV . NInt ) `fmap` get 
               3 -> (NumV . NReal ) `fmap` get 
               5 -> do () <- get
                       return Unit
               6 -> do p1 <- getFull
                       p2 <- getFull
                       return $ PairV p1 p2
               7 -> do n <- idInt `fmap` get
                       --vls <- get
                       vls <- forM [0..n-1] $ const getFull
                       return $ ListV vls
               8 -> do t1 <- get
                       t2 <- get
                       dt <- get
                       vls <- forM [t1,t1+dt..t2] $ const getFull
                       let arr = listArray (0, length vls -1) vls
                       return . SigV t1 t2 dt $ \pt->arr!pt
               9 -> StringV `fmap` get 
               tt -> error $ "unknown type tag: "++show tt

getRaw BoolT = BoolV `fmap` get 
getRaw (NumT (Just RealT)) = (NumV . NReal) `fmap` get 
getRaw (NumT (Just IntT)) = (NumV . NInt) `fmap` get 
getRaw (UnitT) = return Unit
getRaw (StringT) = StringV `fmap` get
getRaw (PairT t1 t2) = do v1 <- getRaw t1
                          v2 <- getRaw t2
                          return $ PairV v1 v2
getRaw (ListT t) = do n <- get :: Get Int
                      ListV `fmap` getManyRaw n t
getRaw (SignalT t) = do t1 <- get
                        t2 <- get
                        dt <- get 
                        let n = round $ (t2-t1)/dt
                        vls <- getManyRaw n t
                        let arr = listArray (0, length vls -1) vls
                        return . SigV t1 t2 dt $ \pt->arr!pt
                        

-- copied from Data.Binary
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


idWord8 :: Word8 -> Word8
idWord8 = id


instance Binary a => Binary (Signal a) where
    get = do t1 <- get
             t2 <- get
             dt <- get 
             let n = round $ (t2-t1)/dt
             vls <- getMany n
             let arr = listArray (0, length vls -1) vls
             return . Signal t1 t2 dt $ \pt->arr!pt
    put (Signal t1 t2 dt sf)= do put t1 
                                 put t2
                                 put dt
                                 mapM_ (\t->put $ sf t) [0..round $ (t2-t1)/dt]

loadReifiedBinary :: (Reify a, Binary a, Show a) => String -> IO [a]
loadReifiedBinary fp = res where
    res = do let expectedTypeTag = typeOfReified (head $ unIO res)
             bs <- L.readFile fp
             let (n, bs1,_) =  runGetState (get) bs 1000
             --return $ idInt n
             let (actualTypeTag, restOfFile, _)  =runGetState (parseTT `fmap` get) bs1 1000
             when (actualTypeTag /= expectedTypeTag) (fail $ "laodReifyBin "++fp++": "++show expectedTypeTag ++ " != "++ show actualTypeTag)
             --putStrLn $ show expectedTypeTag ++ " =?= "++ show actualTypeTag
             let objs =runGet (forM [1..(idInt n)] $ const get) restOfFile
             --let obj =runGet (get) restOfFile 
             --print objs
             return $ objs

unIO :: IO a -> a
unIO = undefined