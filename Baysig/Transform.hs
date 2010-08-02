{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Transform where

import Baysig.Expr
import Baysig.Eval
import Data.List


--lambdasToTheRight = lTTR where 
--    lTTR ((DLet (PVar nm:ps@(_:_)) e):ds) = 
         
removeTopLevelPatterns :: [D] -> [D]
removeTopLevelPatterns allds = rTLP allds where 
   dlets = [(nm,(ps,e)) | DLet (PVar nm:ps) e <- allds]
   repeats = map head $
             filter ((>1) . length) $ 
             group $ map fst dlets
   without1 nm (DLet (PVar nm':ps) e) = nm /= nm'
   without nm = filter (without1 nm)
   rTLP :: [D] -> [D] 
   rTLP [] = []
   rTLP (d@(DLet (PVar nm:ps) e):ds) 
      | nm `elem` repeats
         = mkF nm : rTLP (without nm ds)
      | otherwise = d:rTLP ds 
   rTLP (d:ds) =  d:rTLP ds
   mkF :: String -> D
   mkF nm = let pses = [(ps,e) | (nm',(ps,e)) <- dlets, nm==nm']
                ps = transpose $ map fst pses
                lams ((ps',n):pss') incase | any2 (/=) ps' = ELam (PVar $ "_arg"++show n) $ lams pss' (n:incase)
                                           | otherwise = ELam (head ps') $ lams pss' incase
                lams [] []  = head $ map snd pses
                lams [] [n] = ECase (EVar $ "_arg"++show n) (map (\(ps,e)->(ps!!n,e)) pses)  
            in DLet [PVar nm] $ lams (zip ps [0..]) []

any2 rel [] = False
any2 rel (x:xs) = any (rel x) xs || any2 rel xs