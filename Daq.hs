module Daq where

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
  setupReadWave ch npts