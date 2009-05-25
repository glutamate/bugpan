module Daq where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Statement
import Expr
import Numbers
import Data.HashTable as H
import EvalM
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
  fmap (map realToFrac) $ peekArray npnts ptr

compileAdcSrc rs@(ReadSource nm ("adc":chanS:rtHzS:lenS:_)) = 
    let rtHz= read rtHzS 
        chanNum = read chanS
        len = read lenS
    in [
     RunPrepare $ \env -> do setupInput rtHz
                             promN <- setupInputChannel (AnalogChannel AnalogInput (-10,10) rtHz chanNum) len
                             H.update env "adc_input_promise_number" (NumV . NInt $ promN)
                             return (),
     Trigger $ const internal_trigger,
     RunAfterGo $ const start_cont_acq,
     RunAfterDone $ \env -> do Just (NumV (NInt promN)) <- H.lookup env "adc_input_promise_number" 
                               pts <- retrieveInputWave promN (round (len*(realToFrac rtHz)))
                               H.update env ('#':nm) . SigV 0 len (1/(realToFrac rtHz)) $ \t-> NumV . NReal $ pts!!(round $ t*(realToFrac rtHz))
                               return ()
    ]