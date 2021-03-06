module GenC.Driver where

import GenC.Backend

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
import GHC.Conc
import Data.Char

isLeft (Left _ ) = True
isLeft _ = False
fromRight (Right x) = x

type CompileToken =  (String, String, String, Double, Double)


useRT :: String -> [(String, T)] -> IO CompileToken 
useRT fnm params = do
  ds <- io $ fileDecls fnm []
  useRTds ds params

useRTs :: String -> [(String, T)] -> IO CompileToken
useRTs s params = do
  --liftIO $ print s
  ds <- io $ stringDecls s []
  --liftIO $ print ds
  useRTds ds params

useRTds :: [Declare] -> [(String, T)] -> IO CompileToken
useRTds ds' params = do
  --liftIO $ print ds'
  let eds = safeRunTravM ds' transform
  let ds = fromRight eds 
  let dt = (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001
  let tmax = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1
  let modNm = head [nm | Let (PatVar "moduleName" _) (Const (StringV nm)) <- ds]
  let fileroot =  map toLower modNm --(head $ splitBy '.' fnm)
  let filec = fileroot++".c"
  let gccArgs = if isDynClamp ds
                   then concat ["-I. -I/usr/realtime/include ",
                                "-I/home/tomn/bugpan/GenC ",
                                "-I/usr/src/linux/include -Wall -pipe -D_GNU_SOURCE ",
                                "-L/usr/realtime/lib -lpthread -lkcomedilxrt -lm -lcomedi -o ",
                                fileroot," /home/tomn/bugpan/GenC/dyncal.c ",filec]
                   else "-lm -Wall "++" -o "++fileroot++" "++filec
  --io $print "compile done0"
  --io $ print eds 
  --mapM print ds
  io $ compileToC filec dt tmax ds params 
  --io $print gccArgs
  io $ system $ "gcc "++gccArgs
  --io $print "compile done"
  return (fileroot, modNm, unlines $ map ppDecl ds', tmax, dt)

invokeRT (fnm, modNm, prg, tmax,dt) vals = do
  s <- get
  t0 <- getTnow 
--  rt <- realTime `fmap` get
  let t0' = if t0 < lastTStop s 
               then lastTStop s +1
               else t0 
  Session sessNm _ <- getSession
  let valargs = intercalate " " $ map (ppVal . snd) vals --ideally check ordering
  let cmdStr = "./"++fnm++" "++valargs --bugpanRootDir./"queryCache"./sha++" "++(last $ splitBy '/' sessNm)++" "++show t0' ++" "++valargs
  liftIO . putStrLn $ cmdStr
  liftIO $ system $ cmdStr 
  put $ s { lastTStart = t0',
            lastTStop = t0' + tmax}
  signms <- fmap (filter ("dyn_sig_" `isPrefixOf`)) $ liftIO (getDirContents ".")
  inLast ("running" := ()) 
  inLast ("moduleName" := modNm) 
  inLast ("program" := prg) 
  let initLower (x:xs) = toLower x : xs
  inLast (initLower modNm := ()) 
  let npnts = round $ tmax/dt

  forM signms $ \sigfp-> do
    --io $ print sigfp
    sig <- liftIO $ withBinaryFile sigfp ReadMode $ \h->  do
                                n <- idInt `fmap` binGet h 8
                                --print n
                                arr <- SV.hGet h npnts
                                return $ Signal 0 tmax dt arr Eq
    let nm = drop (length "dyn_sig_") sigfp
    inLastSig (nm :=  sig)
    io $ removeFile sigfp
  return ()
   --
wait :: MonadIO m => Double -> m ()
wait s =  liftIO (threadDelay $ round $ s*1000*1000)