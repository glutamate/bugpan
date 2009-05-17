module Daq where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array

import Comedi.Comedi

setupInput ::  Int -> IO ()
setupInput rtHz = do subdev <- findSubdeviceByType  AnalogInput
                     new_trial (fromIntegral $ subdev) 
                                   (fromIntegral $ rtHz)
                     return ()

setupInputChannel :: Chan-> Double -> IO Int
setupInputChannel ac@(AnalogChannel _ _ _ _) tmax = do
  ch<- actualizeChannel [] ac 
  setupInputChannel ch tmax
setupInputChannel ch tmax = do 
  globFreq <- getGlobalFreq
  let delt = (1.0/(realToFrac globFreq)) -- $ acqRateHz ch'))
  let npts = round $ tmax/delt
  let dn = round (globFreq / realToFrac (acqRateHz ch)) -1
  fromIntegral `fmap` setupReadWave ch npts

retrieveInputWave :: Int -> Int -> IO [Double]
retrieveInputWave nprom npnts = do
  ptr <- get_wave_ptr (fromIntegral nprom)
  fmap realToFrac $ peekArray npnts ptr

{-prom2res (WvProm nm nchan npnts delt dn) 
    = do ptr <- get_wave_ptr (fromInteger nchan)
         lst <- peekArray npnts ptr
	 let newUVec = toU $ map toDouble lst
         --putStr $ "decimate by "++show dn
         return (nm, AnyRes {-. decimate dn -}$ UVecWave (newUVec) 
                                                        delt 
                                                        0.0
                                                        (lengthU newUVec))
         where toDouble :: Real a => a -> Double
               toDouble = realToFrac
-}