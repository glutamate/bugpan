module SrcSinks where

import Expr
import Eval
import EvalM
import Numbers
import Control.Monad
--import Charts
import Control.Concurrent

data Device a = Device {
	initWith :: IO (),
	finalizeWith :: IO (),
	prepareWith :: IO (),
	triggerWith :: IO (),
	flushWith :: IO (),
	unDev :: a
}


emptyDev d = Device ru ru ru ru ru d  
	where ru = return () 

secondsSig = emptyDev $ SrcAnyTimePure (\t->NumV $ NReal t)

printSink = emptyDev $ SinkAnyTime (\t v-> do putStrLn $ "at "++show t++": "++show v)

plotSink 
    = emptyDev $ SinkAllInOneGoAnytimeAnyRate 
      (\vpts-> do
         let pts = map (\(t,v) -> (t, unsafeVToDbl v)) vpts
--         forkIO $ plotGraph (pts%Lines)
         return ()
      )

data SigSrc =   SrcAllInOneGo (IO [V]) Int
		-- | SrcRealTime (IO a)
		| SrcAnyTime (Double -> IO V)
                | SrcAnyTimePure (Double -> V)

data SigSink =   SinkAllInOneGoBefore ([V] -> IO ()) 
		| SinkAllInOneGoAnytime ([V] -> IO ()) 
		| SinkAllInOneGoAnytimeAnyRate ([(Double, V)] -> IO ()) 
		-- | SinkRealTime (a-> IO ())
		| SinkAnyTime (Double -> V -> IO ())
                | SinkRealTime (V->IO ())
                | SinkPull (IO V)

data EventSink = EvtSink (Double -> V -> IO ())

loadBefore (Device _ _ _ _ _ (SinkAllInOneGoBefore _)) = True
loadBefore _ = False

applyVlToSink :: [(Double, V)] -> Device SigSink -> IO ()
applyVlToSink vls (Device _ _ _ _ _  (SinkAnyTime teio)) 
    = forM_ vls $ uncurry teio
applyVlToSink vls (Device _ _ _ _ _  (SinkAllInOneGoAnytime teio)) 
    = teio $ map snd vls
applyVlToSink vls (Device _ _ _ _ _  (SinkAllInOneGoAnytimeAnyRate teio)) 
    = teio (vls)


getVlsFromSig :: Device SigSrc -> IO V
--getVlsFromSig (Device _ _ _ _ _  (SrcAnyTimePure srcfun))
--    = return $ SigV srcfun
getVlsFromSig (Device _ _ _ _ _  (SrcAllInOneGo vlio hz)) = do 
  vls <- vlio
  let dt = 1/realToFrac hz
  return (SigV 0 ((realToFrac $ length vls)*dt) $ \t-> vls!!(round $  t * realToFrac hz))

