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

stdProg nm ln = unlines $ [
 "module "++nm++" where",
 "vm :: Signal Real",
 "vm <* ADC 0",
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
  useRTs stimLegS []
  interactively $ do adjustable "legamp" 4
                     adjustable "legwidth" 0.002
                     adjustable "nerveamp" 4
                     adjustable "nervewidth" 0.002
                     printLn "hello"
                     stimLeg <- use stimLegS
                     printLn "hello2"
                     liftIO $ print stimLeg
                     stimNerve <- use stimNerveS
                     loop [("plot sine", ("ps", tellGnuplot "plot sin(x)")),
                           ("plot cosine", ("pc", tellGnuplot "plot cos(x)")),
                           ("stimulate leg", ("l", invoke stimLeg>>plotvm)),
                           ("stimulate nerve", ("n", invoke stimNerve>>plotvm)),
                           ("plot voltage", ("pv", do iplotSig "vm")),
                           ("show session", ("ss", showSession)),
                           ("new session", ("sn", newSess))
                          ]
                     newline

data InteractS = InteractS {
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
  interactivePlot $ \h -> 
        let initS = InteractS [] "" h (return ())
        in do hSetBuffering stdin NoBuffering
              evalStateT (inLastOrNewSession ima) initS 
              return ()
getKey = liftIO (hSetBuffering stdin NoBuffering >> getChar)
printLn, printS :: String -> InteractM ()
printLn s = liftIO $ do putStrLn s
                        hFlush stdout
printS s = liftIO $ do putStr s
                       hFlush stdout

newline :: InteractM ()
newline=  liftIO $ putChar '\n'    

tellGnuplot :: String -> InteractM ()
tellGnuplot s = do h <- lift $ gnuplotH `fmap` get 
                   liftIO $ tellInteractivePlot  h s

adjustables :: [(String, Double)] -> InteractM ()
adjustables tab = lift $ modify $ \(InteractS vls x y c) -> InteractS (tab++vls)  x y c

adjustable :: String -> Double -> InteractM ()
adjustable nm v = lift $ modify $ \(InteractS vls x y c) -> InteractS ((nm,v):vls)  x y c

loop :: [(String, (String, InteractM ()))] -> InteractM ()
loop actionTable = loop' [] where
  loop' inbuf = do
    when (null inbuf) prompt
    c <- getKey
    case c of 
      '?' -> help >> loop' []
      '+' -> adjust 1.05 >> loop' []
      '-' -> adjust (1/1.05) >> loop' []
      '*' -> adjust 1.30 >> loop' []
      '/' -> adjust (1/1.30) >> loop' []
      'a' -> changeAdjust >> loop' []
      'q' -> cleanUpMess >> return ()
--      'q' -> confirm "really quit (y/n)?" (cleanUpMess >> return ()) (newline >> loop' [])
      _ -> tryAction (inbuf++[c]) 
    return ()
  tryAction s = case lookup s $ map snd actionTable of
                 Nothing -> loop' s
                 Just ma -> newline >> ma >> loop' []
  adjust factor = do InteractS allVls curNm h  c <- lift get
                     let f (nm,v) | nm == curNm = (nm, v*factor)
                                  | otherwise = (nm,v)
                     lift $ put $ InteractS (map f allVls) curNm h c
                     newline 
                                  
  help = do newline >> printLn "Help:"
            forM actionTable $ \(nm,(key,_)) -> printLn $ key ++ ": "++nm
  changeAdjust =  do InteractS allVls curNm h clUp <- lift get
                     newline
                     forM (zip [0..] allVls) $ \(n, (nm,vl)) -> 
                          printLn $ show n ++ ". "++nm++" "++accushow vl
                     c <- getKey
                     let newnm = case c of 
                                   'x' -> ""
                                   _ | c `elem` ['0'..'9'] -> fst $ allVls!!(read $ c:[])
                                     | otherwise -> curNm
                     lift $ put $ InteractS allVls newnm h clUp
                     newline 
  prompt = do InteractS allVls curNm _ _ <- lift get
              liftIO $ case curNm of
                "" -> putStr "> "
                s -> let curVal = fromJust $ lookup s allVls
                     in putStr $ curNm ++"=" ++ accushow curVal ++"> "
              liftIO $ hFlush stdout
                

showSession :: InteractM ()
showSession = do bdir <- (baseDir . qsSess) `fmap` get
                 printLn bdir

newSess :: InteractM ()
newSess = do sess <- liftIO $ newSession sessionsRootDir
             changeToSession sess

  
cleanUpMess :: InteractM ()
cleanUpMess = do 
     InteractS _ _ _ clUp <- lift get
     liftIO clUp
                      
addToCleanUp :: IO () -> InteractM ()
addToCleanUp mio = do is <- lift get
                      lift $ put is {cleanupIO = cleanupIO is >> mio}

iplot :: PlotWithGnuplot a => a -> InteractM ()
iplot x = do
  plines <- liftIO $ multiPlot unitRect x
  let cmdLines = "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  liftIO $ putStrLn cmdLines
  tellGnuplot cmdLines
  addToCleanUp $ cleanupCmds $ map snd plines
  return ()


iplotSig :: String -> InteractM ()
iplotSig nm = do
     s <- signalsDirect nm
     if null s
        then return ()
        else iplot $ [last s]

invoke :: (String, Double, Double) ->  InteractM ()
invoke tok = do 
  vals <- adjVals `fmap` lift get
  invokeRT tok $ map f vals
      where f (nm,dbl) = (nm, NumV $ NReal dbl)

useFile :: String -> InteractM (String, Double, Double)
useFile fnm = do
    adjNms <- (map fst . adjVals) `fmap` lift get
    liftIO $ useRT fnm $ zip adjNms $ repeat (NumT (Just RealT))

use :: String -> InteractM (String, Double, Double)
use s = do
    adjNms <- (map fst . adjVals) `fmap` lift get
    liftIO $ useRTs s $ zip adjNms $ repeat (NumT (Just RealT))

    
interval s = do
  st <- get
  t0 <- getTnow
  let twait = s - (t0 - lastTStart st)
  printLn $ "waiting "++show twait ++" s" 
  wait twait

