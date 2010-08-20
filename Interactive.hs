--module Interactive where
module Main where

import PlotGnuplot
import GHC.Conc as Conc
import System.IO
import Control.Monad.State.Strict
import qualified System.Process as Proc
import Data.Maybe

main = do
  putStrLn "hello"
  interactively $ do adjustable "amplitude" 4
                     loop [("s", tellGnuplot "plot sin(x)"),
                           ("c", tellGnuplot "plot cos(x)")]

data InteractS = InteractS {
      adjVals :: [(String, Double)],
      curAdj :: String,
      gnuplotH :: (Handle, Handle, Handle, Proc.ProcessHandle)
    }

type InteractM a = StateT InteractS IO a

confirm :: String -> InteractM a -> InteractM a -> InteractM a
confirm s yma nma = do newline
                       printLn $ s
                       c <- getKey 
                       case c of 
                         'y' -> yma
                         'n' -> nma
                         _ -> confirm s yma nma


interactively :: InteractM a -> IO ()
interactively ima = do
  interactivePlot $ \h -> 
        let initS = InteractS [] "" h
        in do   hSetBuffering stdin NoBuffering
                evalStateT ima initS 
                return ()
getKey = lift (hSetBuffering stdin NoBuffering >> getChar)
printLn :: String -> InteractM ()
printLn s = lift $ do putStrLn s
                      hFlush stdout



tellGnuplot :: String -> InteractM ()
tellGnuplot s = do h <- gnuplotH `fmap` get 
                   lift $ tellInteractivePlot  h s

adjustables :: [(String, Double)] -> InteractM ()
adjustables tab = modify $ \(InteractS vls x y) -> InteractS (tab++vls)  x y

adjustable :: String -> Double -> InteractM ()
adjustable nm v = modify $ \(InteractS vls x y) -> InteractS ((nm,v):vls)  x y

loop :: [(String, InteractM ())] -> InteractM ()
loop actionTable = loop' [] where
  loop' inbuf = do
    when (null inbuf) prompt
    c <- getKey
    case c of 
      '?' -> help >> loop' []
      '+' -> adjust 1.05 >> loop' []
      '-' -> adjust 0.95 >> loop' []
      'a' -> changeAdjust >> loop' []
      'q' -> newline >> return ()
      _ -> tryAction (c:inbuf) 
    return ()
  tryAction s = case lookup s actionTable of
                 Nothing -> loop' s
                 Just ma -> ma >> loop' []
  adjust factor = do InteractS allVls curNm h  <- get
                     let f (nm,v) | nm == curNm = (nm, v*factor)
                                  | otherwise = (nm,v)
                     put $ InteractS (map f allVls) curNm h
                     newline
                                  
  help = do printLn "Help!"
  changeAdjust =  do InteractS allVls curNm h <- get
                     newline
                     forM (zip [0..] allVls) $ \(n, (nm,_)) -> printLn $ show n ++ ". "++nm
                     c <- getKey
                     let newnm = case c of 
                                   'x' -> ""
                                   _ | c `elem` ['0'..'9'] -> fst $ allVls!!(read $ c:[])
                                     | otherwise -> curNm
                     put $ InteractS allVls newnm h
                     newline
  prompt = do InteractS allVls curNm _ <- get
              lift $ case curNm of
                "" -> putStr "> "
                s -> let curVal = fromJust $ lookup s allVls
                     in putStr $ curNm ++"=" ++ show curVal ++"> "
              lift $ hFlush stdout
                
newline :: InteractM ()
newline=  lift $ putChar '\n'    

  
                      