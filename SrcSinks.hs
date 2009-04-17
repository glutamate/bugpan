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

printSnk = emptyDev $ SnkAnyTime (\t v-> do putStrLn $ "at "++show t++": "++show v)

plotSnk 
    = emptyDev $ SnkAllInOneGoAnytimeAnyRate 
      (\vpts-> do
         let pts = map (\(t,v) -> (t, unsafeVToDbl v)) vpts
--         forkIO $ plotGraph (pts%Lines)
         return ()
      )

data SigSrc =   SrcAllInOneGo (IO [V]) Int
		-- | SrcRealTime (IO a)
		| SrcAnyTime (Double -> IO V)
                | SrcAnyTimePure (Double -> V)

data SigSnk =   SnkAllInOneGoBefore ([V] -> IO ()) 
		| SnkAllInOneGoAnytime ([V] -> IO ()) 
		| SnkAllInOneGoAnytimeAnyRate ([(Double, V)] -> IO ()) 
		-- | SnkRealTime (a-> IO ())
		| SnkAnyTime (Double -> V -> IO ())

loadBefore (Device _ _ _ _ _ (SnkAllInOneGoBefore _)) = True
loadBefore _ = False


applyVlToSnk :: [(Double, V)] -> Device SigSnk -> IO ()
applyVlToSnk vls (Device _ _ _ _ _  (SnkAnyTime teio)) 
    = forM_ vls $ uncurry teio
applyVlToSnk vls (Device _ _ _ _ _  (SnkAllInOneGoAnytime teio)) 
    = teio $ map snd vls
applyVlToSnk vls (Device _ _ _ _ _  (SnkAllInOneGoAnytimeAnyRate teio)) 
    = teio (vls)


getVlsFromSig :: Device SigSrc -> IO V
getVlsFromSig (Device _ _ _ _ _  (SrcAnyTimePure srcfun))
    = return $ SigV srcfun
getVlsFromSig (Device _ _ _ _ _  (SrcAllInOneGo vlio hz)) = do 
  vls <- vlio
  return (SigV $ \t-> vls!!(round $  t * realToFrac hz))

