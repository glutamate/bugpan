--module Interactive where
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import PlotGnuplot
import GHC.Conc as Conc
import System.IO
import Control.Monad.State.Lazy
import qualified System.Process as Proc
import Data.Maybe
import QueryPlots
import QueryTypes
import Query
import Database
import GenC.Driver
import EvalM
import Numbers 
import NewSignal
import System.Environment
import TNUtils
import Data.List

dcmd = unlines $ [
 "module RecordDCMD where",
 "ec :: Signal Real",
 "ec <* ADC 1",
 "ec *> store \"\"",
 "_tmax = 1",
 "_dt = 5.0e-5",
 "stim = {: 0 :}",
 "stim *> DAC 0",  "legwidth=0.001", "legamp=4",
 "nervewidth=0.001","nerveamp=4"]

recBothS = unlines $ [
 "module RecordBoth where",
 "rawvm :: Signal Real",
 "rawvm <* ADC 0",
 "vm = {: <: rawvm :> * 20 :}",
 "vm *> store \"\"",
 "ec :: Signal Real",
 "ec <* ADC 1",
 "ec *> store \"\"",
 "_tmax = 1",
 "_dt = 5.0e-5",
 "stim = {: 0 :}",
 "stim *> DAC 0",  "legwidth=0.001", "legamp=4",
 "nervewidth=0.001","nerveamp=4"]


stdProg nm ln = unlines $ [
 "module "++nm++" where",
 "rawvm :: Signal Real",
 "rawvm <* ADC 0",
 "vm = {: <: rawvm :> * 20 :}",
 "step :: Real -> Real -> Real -> Signal Real",
 "step t0 width amp = {: if (<:seconds :> > t0) && (<:seconds :> < (t0+width)) then amp else 0 :}",
 "legwidth :: Real","legamp :: Real",
 "nervewidth :: Real","nerveamp :: Real",
 "legwidth=0.001", "legamp=4",
 "nervewidth=0.001","nerveamp=4",
 "stim :: Signal Real",
 "vm *> store \"\"",
 "_tmax = 0.1",
 "_dt = 5.0e-5"] ++ ln

stimLegS = stdProg "StimLeg" 
              ["stim = step 0.01 legwidth legamp",
               "stim*> DAC 0"]
stimNerveS = stdProg "StimNerve" 
               ["stim = step 0.01 nervewidth nerveamp",
                "stim *> DAC 1"]

plotvm = iplotSig "vm"

main = do
  --useRTs stimLegS []
  interactively $ do adjustable "legamp" 4
                     adjustable "legwidth" 0.002
                     adjustable "nerveamp" 4
                     adjustable "nervewidth" 0.002
                     stimLeg <- use stimLegS
                     stimNerve <- use stimNerveS
                     recdcmd <- use dcmd
                     recBoth <- use recBothS
                     
                     action "stimulate leg" "l" $ invoke stimLeg>>plotvm
                     action "record nerve" "d" $ invoke recdcmd>> iplotSig "ec"
                     action "record both" "b" $ invoke recBoth>> iplot2Sigs "vm" "ec"
                     action "stimulate nerve" "n" $ invoke stimNerve>>plotvm
                     action "show session" "ss" $ showSession
                     action "new session" "sn" $ newSess
                     action "help" "?" help
                     action "increase 30%" "*" $ adjustBy 1.30
                     action "increase 5%" "+" $ adjustBy 1.05
                     action "decrease 5%" "-" $ adjustBy (1/1.05)
                     action "decrease 30%" "/" $ adjustBy (1/1.30)
                     action "make note" "n" $ do s <- getLn "Note> "
                                                 mkNote s
                     changeAdjustAction "legamp" "1"
                     changeAdjustAction "legwidth" "2"
                     changeAdjustAction "nerveamp" "3"
                     changeAdjustAction "nervewidth" "4"

                     loop
  
data InteractS = InteractS {
      actions :: [(String, (String, InteractM ()))],
      adjVals :: [(String, Double)],
      curAdj :: String,
      gnuplotH :: (Handle, Handle, Handle, Proc.ProcessHandle),
      cleanupIO :: IO ()
    }

type InteractM a = StateT QState (StateT InteractS IO) a

confirm :: String -> InteractM a -> InteractM a -> InteractM a
confirm s yma nma = do newline
                       printS $ s
                       c <- getKey 
                       case c of 
                         'y' -> yma
                         'n' -> nma
                         _ -> confirm s yma nma


interactively :: InteractM () -> IO ()
interactively ima = do
  --w <- initScr
  interactivePlot $ \h -> 
        let initS = InteractS [] [] "" h (return ())
        in do hSetBuffering stdin NoBuffering
              evalStateT (inLastOrNewSession ima) initS 
              return ()
  bufmode <- hGetBuffering stdin
  when (bufmode /= LineBuffering) $ hSetBuffering stdin LineBuffering 
  --endWin
getKey = liftIO (getChar)
printLn, printS :: String -> InteractM ()
printLn s = liftIO $ do putStrLn s
                        hFlush stdout
printS s = liftIO $ do putStr s
                       hFlush stdout

newline :: InteractM ()
newline=  liftIO $ do bufmode <- hGetBuffering stdin
                      when (bufmode /= NoBuffering) $ hSetBuffering stdin NoBuffering 
                      putChar '\n'    
getLn :: String -> InteractM String
getLn s = do printS s 
             liftIO $ do 
                bufmode <- hGetBuffering stdin
                when (bufmode /= LineBuffering) $ hSetBuffering stdin LineBuffering 
                s<- getLine
                hSetBuffering stdin NoBuffering 
                return s

mkNote :: String -> InteractM ()
mkNote str = do
  t1 <- getTnow
  sess@(Session bdir _) <- getSession
  liftIO $ saveInSession sess "note" t1 0.001 $ pack [(t1,str)]
  return ()

tellGnuplot :: String -> InteractM ()
tellGnuplot s = do h <- lift $ gnuplotH `fmap` get 
                   liftIO $ tellInteractivePlot  h s

adjustables :: [(String, Double)] -> InteractM ()
adjustables tab = lift $ modify $ \(InteractS as vls x y c) -> InteractS as (tab++vls)  x y c

adjustable :: String -> Double -> InteractM ()
adjustable nm v = lift $ modify $ \(InteractS as vls x y c) -> InteractS as ((nm,v):vls)  x y c

action :: String -> String -> InteractM () -> InteractM ()
action nm key act = lift $ modify $ \(InteractS as vls x y c) -> InteractS ((nm,(key,act)):as) vls x y c

changeAdjustAction :: String -> String -> InteractM()
changeAdjustAction nm key = action ("adjust "++nm) key $ changeAdjust nm

help :: InteractM ()
help = do actionTable <- (reverse . actions) `fmap` lift get
          termcols::Int <- read `fmap` liftIO (getEnv "COLUMNS")
--          newline
          let maxdesclen = foldr max 0 $ map (length . fst) actionTable
          let maxkeylen = foldr max 0 $ map (length . fst . snd) actionTable
          let ncols = termcols `div` (maxdesclen+maxkeylen+5)
          let padStr n s = s ++ replicate (n- length s) ' '
          let showEntry (nm,(key, _)) = padStr (maxkeylen+2) (key++":") ++  padStr (maxdesclen+3) nm 
          printLn "Help:"
          forM_ (inChunksOf ncols actionTable ) $ \acts -> do 
               forM_ acts $ printS . showEntry
               newline
          


loop :: InteractM ()
loop = do loop' [] where
  loop' inbuf = do
    actionTable <- actions `fmap` lift get
    let  tryAction s = case lookup s $ map snd actionTable of
                 Nothing -> loop' s
                 Just ma -> newline >> ma >> loop' []
    when (null inbuf) prompt
    c <- getKey
    case c of 
      '\n' -> loop' []
      'q' -> cleanUpMess >> newline >> return ()
      '!' -> do newline
                reps <- getLn "Repeats> "
                isi <- getLn "ISI> "
                act <- getLn "Command> "
                case lookup act $ map snd actionTable of
                 Nothing -> loop' []
                 Just ma -> repAct (safeRead reps) (safeRead isi) ma >> loop' []
           
                
      _ -> tryAction (inbuf++[c]) 
    return ()
  adjust factor = do InteractS as allVls curNm h c <- lift get
                     let f (nm,v) | nm == curNm = (nm, v*factor)
                                  | otherwise = (nm,v)
                     lift $ put $ InteractS as (map f allVls) curNm h c
                     newline 
                                  
  prompt = do InteractS as allVls curNm _ _ <- lift get
              liftIO $ case curNm of
                "" -> putStr "> "
                s -> let curVal = fromJust $ lookup s allVls
                     in putStr $ curNm ++"=" ++ accushow curVal ++"> "
              liftIO $ hFlush stdout
                
repAct :: Maybe Int -> Maybe Double -> InteractM () -> InteractM ()
repAct (Just 0) _ _ = do return ()
repAct (Just reps) (Just isi) ma  = interval isi $ ma >> repAct (Just $ reps-1) (Just isi) ma
repAct _ _ _ = do return ()
  

changeAdjust :: String -> InteractM ()
changeAdjust nm = do
  InteractS as allVls curNm h clUp <- lift get
  when (nm `elem` map fst allVls) $
      lift $ put $ InteractS as allVls nm h clUp

adjustBy :: Double -> InteractM ()
adjustBy factor = do InteractS as allVls curNm h c <- lift get
                     let f (nm,v) | nm == curNm = (nm, v*factor)
                                  | otherwise = (nm,v)
                     lift $ put $ InteractS as (map f allVls) curNm h c           

showSession :: InteractM ()
showSession = do bdir <- (baseDir . qsSess) `fmap` get
                 printLn bdir

newSess :: InteractM ()
newSess = do sess <- liftIO $ newSession sessionsRootDir
             changeToSession sess

  
cleanUpMess :: InteractM ()
cleanUpMess = do 
     clUp <- cleanupIO `fmap` lift get
     liftIO clUp
                      
addToCleanUp :: IO () -> InteractM ()
addToCleanUp mio = do is <- lift get
                      lift $ put is {cleanupIO = cleanupIO is >> mio}

iplot :: PlotWithGnuplot a => a -> InteractM ()
iplot x = do
  plines <- liftIO $ multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  --liftIO $ putStrLn cmdLines
  tellGnuplot cmdLines
  addToCleanUp $ cleanupCmds $ map snd plines
  return ()
 
iplotSig :: String -> InteractM ()
iplotSig nm = do
     s <- signalsDirect nm
     if null s
        then return ()
        else iplot $ map (sigZero . sigCutLast 0.001) [last s]

iplot2Sigs :: String -> String -> InteractM ()
iplot2Sigs nm1 nm2 = do
     s1 <- signalsDirect nm1
     s2 <- signalsDirect nm2
     if null s1 || null s2
        then return ()
        else iplot $ map (sigZero . sigCutLast 0.001) [last s1] :==: map (sigZero . sigCutLast 0.001) [last s2] 

invoke :: CompileToken ->  InteractM ()
invoke tok = do 
  vals <- adjVals `fmap` lift get
  let f (nm,dbl) = (nm, NumV $ NReal dbl)
  invokeRT tok $ map f vals
  forM_ vals $ \(nm, v) -> inLast (nm := v) 

useFile :: String -> InteractM CompileToken
useFile fnm = do
    adjNms <- (map fst . adjVals) `fmap` lift get
    liftIO $ useRT fnm $ zip adjNms $ repeat (NumT (Just RealT))

use :: String -> InteractM CompileToken
use s = do
    adjNms <- (map fst . adjVals) `fmap` lift get
    liftIO $ useRTs s $ zip adjNms $ repeat (NumT (Just RealT))

interval :: Double -> InteractM () -> InteractM ()
interval s action = do
  st <- get
  t0 <- getTnow
  let twait = max 0 $ s - (t0 - lastTStart st)
  let mswait = round $ twait * 1000
  printLn $ "waiting "++show twait ++" s"
  inAvail <- waitForInput (min 500 mswait) 
  case () of _ | inAvail ->  return ()
               | twait < 0.5 -> action
               | otherwise -> interval s action

falseStart :: InteractM ()
falseStart = do
  tnow <- getTnow
  st <- get
  put $ st { lastTStart = tnow }

waitForInput :: Int -> InteractM Bool
waitForInput ms = do
  liftIO (threadDelay $ ms*1000)
  --liftIO $ hFlush stdin
  inReady <- liftIO $ hReady stdin

  return inReady

waitForInput' :: Int -> InteractM Bool
waitForInput' ms = do
  liftIO $ hWaitForInput stdin ms
