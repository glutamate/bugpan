module Driver where

import Backend

import QueryTypes
--import QueryRun
import Parse
import TNUtils 
import PrettyPrint
import Query
import System.Directory
import System.Cmd
import Transform
import Traverse
import Expr hiding (Eq)
import EvalM
import Numbers
import Control.Monad
import Control.Monad.State.Lazy
import Database
import Data.List
import System.IO
import ValueIO
import qualified Data.StorableVector as SV
import NewSignal


useRT ::  MonadIO m => String -> [(String, T)] -> StateT QState m (String, Double, Double)
useRT fnm params = do
  ds' <- io $ fileDecls fnm []
  let ds = let runTM = runTravM ds' [] in snd . runTM $ transform
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  let tmax = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let fileroot = (head $ splitBy '.' fnm)
  let filec = fileroot++".c"
  let gccArgs = if isDynClamp ds
                   then concat ["-I. -I/usr/realtime/include ",
                                "-I/usr/src/linux/include -Wall -pipe -D_GNU_SOURCE ",
                                "-L/usr/realtime/lib -lpthread -lkcomedilxrt -lm "]
                   else "-lm -Wall "
  io $ compileToC filec dt tmax ds [] 
  io $ system $ "gcc "++gccArgs++" -o "++fileroot++" "++filec
  return (fileroot, tmax, dt)

invokeRT (fnm, tmax,dt) params = do
  s <- get
  t0 <- getTnow 
  rt <- realTime `fmap` get
  let t0' = if t0 < lastTStop s 
               then lastTStop s +1
               else t0 
  Session sessNm _ <- getSession
  --let valargs = intercalate " " $ map (ppVal . snd) vals --ideally check ordering
  let cmdStr = "./"++fnm --bugpanRootDir./"queryCache"./sha++" "++(last $ splitBy '/' sessNm)++" "++show t0' ++" "++valargs
  liftIO . putStrLn $ cmdStr
  liftIO $ system $ cmdStr 
  put $ s { lastTStart = t0',
            lastTStop = t0' + tmax}
  signms <- fmap (filter ("dyn_sig_" `isPrefixOf`)) $ liftIO (getDirContents ".")
  inLast ("running" := ()) 
  let npnts = round $ tmax/dt

  forM signms $ \sigfp-> do
    io $ print sigfp
    sig <- liftIO $ withBinaryFile sigfp ReadMode $ \h->  do
                                n <- idInt `fmap` binGet h 8
                                print n
                                arr <- SV.hGet h npnts
                                return $ Signal 0 tmax dt arr Eq
    let nm = drop (length "dyn_sig_") sigfp
    inLastSig (nm :=  sig)
    io $ removeFile sigfp
  return ()
   
