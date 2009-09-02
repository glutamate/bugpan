module CompiledSrcsSinks where

import EvalM
import BuiltIn
import PrettyPrint
import TNUtils

data Src = Src { srcName :: String,
                 srcArgT :: T,
                 srcOutT :: T,
                 srcImpModule :: [String],
                 srcCode :: SrcCode }
           deriving Show

data SrcCode = SrcOnce (String) --type tmax -> dt -> IO (a)
             | SrcRealTimeSig (String) --type t -> dt -> IO (a) where sourcetype = signal a

instance Show SrcCode where
    show (SrcOnce _) = "SrcOnce"
    show (SrcRealTimeSig _) = "SrcRealTimeSig"

data Sink  = Sink { snkName :: String,
                    snkArgT :: T,
                    snkInT :: T,
                    snkImpModule :: [String],
                    snkCode :: V->String->String }

lookupSrc nm = safeHead [s | s@(Src nm1 _ _ _ _) <- srcs, nm == nm1]

srcs = [
 Src "uniform1" (PairT realT realT) (realT) ["System.Random"]
     $ SrcOnce ("(\\_ _ (lo,hi) -> randomRIO (lo,hi))"),
 Src "uniform" (PairT realT realT) (SignalT realT) ["System.Random"]
     $ SrcRealTimeSig ("(\\_ _ (lo,hi)-> randomRIO (lo,hi))"),
 Src "poisson1" (realT) (realT) ["RandomSources"]
     $ SrcOnce ("poisson1"),
 Src "poisson" (realT) (ListT (PairT realT UnitT)) ["RandomSources"]
     $ SrcOnce ("poisson")]


snks = []
