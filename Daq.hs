{-# LANGUAGE CPP #-}

module Daq where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Statement
import Expr
import Numbers
import Data.HashTable as H
import EvalM
#ifndef NODAQ
import Comedi.Comedi
#endif
import Control.Concurrent hiding (Chan)
import Data.Array

#ifndef NODAQ
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
  print ch
  fromIntegral `fmap` setupReadWave ch npts

retrieveInputWave :: Int -> Int -> IO [CDouble]
retrieveInputWave nprom npnts = do
  ptr <- get_wave_ptr (fromIntegral nprom)
  {-dbls <- fmap (map realToFrac) $ -}
  peekArray npnts ptr
  --print $ take 10 dbls
  --return dbls


compileAdcSrc rs@(ReadSource nm ("adc", Const (PairV (PairV chanS rtHzS) lenS))) = 
    let rtHz= unsafeReify rtHzS 
        chanNum =  unsafeReify chanS
        len =  unsafeReify lenS
    in [
     RunPrepare $ \env -> do setupInput rtHz
                             --print "hello world"
                             promN <- setupInputChannel (AnalogChannel AnalogInput (-10,10) chanNum rtHz) len
                             putStrLn $"primise num "++show promN
                             H.update env "adc_input_promise_number" (NumV . NInt $ promN)
                             prepare_cont_acq
                             return (),
     Trigger $ const internal_trigger,
     RunAfterGo $ \env-> do forkIO start_cont_acq
                            return (),
     RunAfterDone $ \env -> do waitUntilAcqDone
                               Just (NumV (NInt promN)) <- H.lookup env "adc_input_promise_number" 
                               putStrLn $"promise num "++show promN
                               let npts = round (len*(realToFrac rtHz))
                               pts <- fmap (array (0,npts) . zip [0..npts-1] . map realToFrac) $! retrieveInputWave promN npts
                               let toV = NumV . NReal 
                               let sf idx = if idx<0 
                                               then toV $ pts!0
                                               else if idx<npts
                                                       then  toV $ pts!idx
                                                       else toV $ pts!(npts-1)
                               H.update env ('%':nm) $ SigV 0 (len) (1/(realToFrac rtHz)) sf
                               free_trial_results
                               return ()
    ]

compileAdcSrc rs = error $ "compileAdcSrc :"++show rs
#else
compileAdcSrc rs = error $ "compileAdcSrc ("++show rs++") compiled with NODAQ"
#endif
