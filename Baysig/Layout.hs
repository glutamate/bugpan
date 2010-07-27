{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Layout where

import Baysig.Lexer

addDeclEnds :: [(Tok,Pos)] -> [(Tok, Pos)]
addDeclEnds = aDE Nothing             
   where aDE Nothing [] = []
         aDE (Just pos) [] = [(EndOf Declaration, pos)]
         aDE Nothing ((t,(x,y)):ts) = (t,(x,y)) : aDE (Just (x,y)) ts
         aDE (Just (dx, dy)) ((t,(x,y)):ts) 
             | x <= dx =  (EndOf Declaration, (x,y)) : (t,(x,y)) : aDE (Just (x, y)) ts
             | otherwise = (t,(x,y)) : aDE (Just (dx, dy)) ts