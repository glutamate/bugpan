module CompiledSrcsSinks where

import EvalM
import BuiltIn
import PrettyPrint

data Src = Src { srcName :: String,
                 srcArgT :: T,
                 srcOutT :: T,
                 srcImpModule :: [String],
                 srcCode :: V->String }

data Sinks = Sink { snkName :: String,
                    snkArgT :: T,
                    snkInT :: T,
                    snkImpModule :: [String],
                    snkCode :: V->String->String }

srcs = [
 Src "uniformVal" (PairT realT realT) (realT) []
     $ \(PairV lo hi) -> "randomRIO ("++ppVal lo++","++ppVal hi++")"]

snks = []
                 