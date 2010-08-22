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

main = do
  interactively $ do adjustable "amplitude" 4
                     loop [("plot sine", ("ps", tellGnuplot "plot sin(x)")),
                           ("plot cosine", ("pc", tellGnuplot "plot cos(x)")),
                           ("plot voltage", ("pv", do vm <- take 2 `fmap` signalsDirect "vm"
                                                      liftIO $ print vm
                                                      iplot $ vm)),
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
              let is::StateT InteractS IO () = inLastOrNewSession ima
              evalStateT is initS 
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

