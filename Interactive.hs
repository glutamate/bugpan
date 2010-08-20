--module Interactive where
module Main where

import PlotGnuplot
import GHC.Conc as Conc
import System.IO

main = do
  putStrLn "hello"
  hSetBuffering stdin NoBuffering
  interactivePlot $ \h -> do
         tellInteractiveSession h "plot sin(x)"
         mainLoop h

mainLoop h = do
  c <- getChar
  case c of 
    's' -> tellInteractiveSession h "plot sin(x)" >> mainLoop h
    'c' -> tellInteractiveSession h "plot cos(x)" >> mainLoop h
    'x' -> return ()
    _ -> mainLoop h

