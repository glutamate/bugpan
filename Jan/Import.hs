{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TNUtils
import Data.List
import qualified Data.StorableVector as SV
import System.Time
import Database
import EvalM hiding (ListT)
import Query
import QueryTypes
import QueryUtils
import Math.Probably.FoldingStats
import ValueIO
import Numbers
import System.Environment
import Data.Ord
import Data.Maybe
import Control.Monad 
import Data.Unique
import System.Cmd
import System.Directory
import NewSignal

{-

ghc --make Import && ./Import

-}

main = do 
  files <- filter (".DAT" `isSuffixOf`) `fmap` getDirectoryContents "."
  forM_ (files) $ \fnm -> do
         putStrLn fnm
         let sessNm = takeWhile (/='.') fnm
         conts <- readFile fnm
         let sigLists =  readMatrix $ lines $ conts
         let sigs= map (listToSig 0.02 0) sigLists
         let maxLength = foldl1 (max) $ map sigT2 sigs
         let angle = map (\(s,t) -> shift t s) $ zip sigs [0,maxLength+1..]
             running = map sigDur angle             
         inApproxSession ("new:"++sessNm) $ do
                                   storeAs "angle" angle
                                   storeAs "running" running
                                   storeAs "moduleName" (tag "JanReal" running)
                                   storeAs "janReal" (running)
                                   return ()
         --print running
         --print sigs
                     

unGerman ',' = '.'
unGerman c = c

--readMatrix :: Read a => [String] -> ([[Double]])
readMatrix lns = 
    let line1 = splitBy '\t' $ head lns
        ncols  = length line1 - 1
        readln ln =  do let cols = drop 1 $ splitBy '\t' ln
                        numbers::[Double] <- mapM (safeRead . map unGerman) cols
                        if length numbers == ncols
                           then return numbers
                           else Nothing
        readall = transpose $  catMaybes $ map readln lns
    in readall