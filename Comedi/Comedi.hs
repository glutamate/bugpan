{-# INCLUDE "comedilib.h" #-}
{-# INCLUDE "comedi.h" #-}
{-# INCLUDE "comedi_hs_helper.c" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Comedi.Comedi where
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List
import GHC.Conc (threadDelay)


{- foreign import ccall safe "comedilib.h comedi_open"
  comedi_open :: CString -> IO (Ptr ())

foreign import ccall safe "comedilib.h comedi_close"
  comedi_close :: Ptr () -> IO CInt
-}
foreign import ccall safe "comedilib.h comedi_get_board_name"
  comedi_get_board_name :: Ptr () -> IO CString

foreign import ccall safe "comedilib.h comedi_find_subdevice_by_type"
  comedi_find_subdevice_by_type :: Ptr () -> CInt -> CInt -> IO CInt

foreign import ccall safe "comedilib.h comedi_find_range"
  comedi_find_range :: Ptr () -> CInt -> CInt -> CInt -> CDouble -> CDouble ->IO CInt

foreign import ccall safe "comedilib.h comedi_dio_write"
  comedi_dio_write :: Ptr () -> CInt -> CInt -> CInt->IO CInt

foreign import ccall safe "comedilib.h comedi_dio_config" 
  comedi_dio_config :: Ptr () -> CInt -> CInt -> CInt->IO CInt

foreign import ccall safe "comedi_hs_helper.h dio_set_bits"
  dio_set_bits :: CInt -> CInt -> CInt-> IO ()

foreign import ccall safe "comedi_hs_helper.h new_trial"
  new_trial :: CInt -> CDouble -> IO CInt

foreign import ccall safe "comedi_hs_helper.h get_comedi_ptr"
  get_comedi_ptr :: IO (Ptr ())

foreign import ccall safe "comedi_hs_helper.h new_trial_out"
  new_trial_out :: CInt -> CDouble -> IO CInt

foreign import ccall safe "comedi_hs_helper.h setup_write_wave"
  setup_write_wave :: CInt -> CInt -> CInt -> CInt-> Ptr (CDouble) -> IO ()

foreign import ccall safe "comedi_hs_helper.h setup_read_wave"
  setup_read_wave :: CInt -> CInt -> CInt -> CInt-> IO CInt

foreign import ccall safe "comedi_hs_helper.h start_cont_acq"
  start_cont_acq :: IO ()

foreign import ccall safe "comedi_hs_helper.h prepare_cont_acq"
  prepare_cont_acq :: IO ()

foreign import ccall safe "comedi_hs_helper.h internal_trigger"
  internal_trigger :: IO ()

foreign import ccall safe "comedi_hs_helper.h start_cont_output"
  start_cont_output :: IO ()

foreign import ccall safe "comedi_hs_helper.h get_wave_ptr"
  get_wave_ptr :: CInt -> IO (Ptr (CDouble))

foreign import ccall safe "comedi_hs_helper.h free_trial_results"
  free_trial_results :: IO ()

foreign import ccall safe "comedi_hs_helper.h subdev_type"
  subdev_type :: CInt -> CInt

foreign import ccall safe "comedi_hs_helper.h getGlobalFreq"
  get_global_freq :: IO CDouble

foreign import ccall safe "comedi_hs_helper.h my_find_subdevice_by_type"
  my_find_subdevice_by_type :: CInt -> CInt -> IO CInt

foreign import ccall safe "comedi_hs_helper.h get_running"
  get_running :: IO CInt


waitUntilAcqDone :: IO ()
waitUntilAcqDone = do r <- get_running
                      case r of
                        0 -> return ()
                        1 -> do threadDelay $ 25*1000
                                waitUntilAcqDone


{-open :: String -> IO (Ptr ())
open fn = withCString fn comedi_open

close :: (Ptr ()) -> IO ()
close dvptr = do comedi_close dvptr
                 return ()
-}
{-readVolts :: Chan -> IO Double
readVolts (AIChan devptr subd chan rng _ _)
          = return . realToFrac =<< read_volts devptr (fI subd) (fI chan) (fI rng)
-}
setupReadWave (AIChan subd chan rng _ ) npnts 
    = setup_read_wave (fI subd) (fI chan) (fI rng) (fI npnts)
setupWriteWave (AOChan subd chan rng _ ) pts
    = do withArray pts $ setup_write_wave (fI subd) (fI chan) (fI rng) (fI $ length pts) 
setupWriteWave och _ = putStrLn "!!!! other write channel" >> print och

getGlobalFreq :: IO Double
getGlobalFreq = return . realToFrac =<<get_global_freq

--int comedi_find_range(comedi_t * device, unsigned int subdevice, unsigned int channel, unsigned int unit, double min, double max);
findRange :: Int -> Int -> (Rational,Rational) ->IO Int
findRange sd ch  minmax = do dvptr <- get_comedi_ptr
                             r <- comedi_find_range dvptr (fI sd) (fI ch) 0 (rTF $ uncurry min minmax) (rTF $ uncurry max minmax) -- 0 = Volts, comedi.h
                             return $ fromIntegral r 
                                        

findSubdeviceByType :: SubDevType -> IO Int
findSubdeviceByType sdtp = do sdNum <- my_find_subdevice_by_type (subdev_type (fromIntegral $ subDevTypeToInt sdtp)) 0
                              putStrLn$  "findSubdeviceByType "++(show sdtp ++ "=>" ++show sdNum)
                              return $ fromIntegral sdNum

fI = fromIntegral
rTF = realToFrac

data SubDevType = AnalogInput | AnalogOutput | DigitalInput | DigitalOutput | DigitalIO deriving Show

subDevTypeToInt AnalogInput = 10
subDevTypeToInt AnalogOutput = 11
subDevTypeToInt DigitalOutput = 12
subDevTypeToInt DigitalInput = 13
subDevTypeToInt DigitalIO = 14

isAnalog AnalogInput = True
isAnalog AnalogOutput = True
isAnalog _ = False

--------------

{-data ChanDesc = AnalogChannel String SubDevType (Rational,Rational) Int Int
	      | DigitalChannel String SubDevType Int -}

data Chan = AIChan { subDev ::Int,  chanNum :: Int, range:: Int, acqRateHz :: Int }
	  | AOChan { subDev ::Int,  chanNum :: Int, range:: Int, acqRateHz :: Int }
	  | DIOChan { subDev ::Int,  chanNum :: Int, direction :: Int}
	  | AnalogChannel { subDevType :: SubDevType, rangePair :: (Rational,Rational),  acqRateHz :: Int, chanNum :: Int }
	  | DigitalChannel { subDevType :: SubDevType , chanNum :: Int }
	    deriving Show

{-
instance Show Chan where
    show (AIChan devptr subd chan rng rate devnm) 
	= "RealChan "++show devnm++" sub "++ show subd++" chan "++ show chan++" rng "++ show rng
    show (ChanDesc devnm subd chan rng rate) 
	= "ChanDesc "++devnm++" sub "++ show subd++" chan "++ show chan++" rng "++ show rng
-}
--actualizeWithPtr :: Ptr () -> Chan -> Chan 
--actualizeWithPtr p nc@(DigitalChannel devname subDevType chan) 

actualizeChannels :: [Chan] -> [Chan] -> IO [Chan]
actualizeChannels [] existing =  	return existing
actualizeChannels (c:cs) existing = do 	nc <- actualizeChannel existing c
					actualizeChannels cs (nc:existing)

actualizeChannel existing nc@(AnalogChannel subDevType rngSpan chanNum rtHz) 
           = do --devPtr <- getDevPtr existing nc
		sdt <- findSubdeviceByType subDevType
                r <- findRange sdt chanNum rngSpan
                --prints "new chan " $ Chan devPtr sdt chanNum rng rtHz devnm
                case subDevType of 
                  AnalogInput -> return $ AIChan sdt chanNum r rtHz 
                  AnalogOutput -> return $ AOChan sdt chanNum r rtHz 

actualizeChannel existing nc@(DigitalChannel subDevType chan) 
           = do devPtr <- get_comedi_ptr
		sdt <- findSubdeviceByType DigitalIO
		let dir = case subDevType of
				DigitalOutput -> 1 
				DigitalInput -> 0
		comedi_dio_config devPtr (fI sdt) (fI chan) (fI dir)
		--putStrLn "done actualizig dig"
                return $ DIOChan sdt chan dir 

actualizeChannel existing c = return c


setDigPin :: Chan -> Int -> IO ()
setDigPin (DIOChan sdt chan dir) val 
    = do devPtr <- get_comedi_ptr
         comedi_dio_write devPtr (fI sdt) (fI chan) (fI val)
	 return () 

beep :: Chan -> IO ()
beep ch = do setDigPin ch 1
	     threadDelay 1000
	     setDigPin ch 0


prints s1 s2 = putStrLn (s1++": "++show s2)


multiDigPulse :: [Chan] -> IO ()
multiDigPulse chs  = do setMultipleDigOuts chs 1
                        threadDelay 1000
                        setMultipleDigOuts chs 0

setMultipleDigOuts :: [Chan]  -> Int -> IO ()
setMultipleDigOuts chs@((DIOChan subD _ _ ):_) val 
                   = dio_set_bits (fI subD) (fI writeMask) (fI bitField) 
                        where powersOf2 = iterate (*2) 1
                              writeMask = sum $ map (powersOf2 !!) pinlist
                              bitField = writeMask * val
                              pinlist = map chanNum chs

setMultipleDigOuts chs@((DigitalChannel _ _ ):_) val 
                   = do achs <- actualizeChannels chs []
			setMultipleDigOuts achs val
					
