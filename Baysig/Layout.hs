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

addLetEnds :: [(Tok,Pos)] -> [(Tok, Pos)] -- FIXME: nested
addLetEnds = aLE []             
   where aLE _ [] = []
         aLE stk ((Id "let",(x',y')):(t,(x,y)):ts) = (Id "let",(x',y')) : (t,(x,y)) : aLE ((x,y):stk) ts
         aLE [] ((t,(x,y)):ts) = (t,(x,y)) : aLE [] ts
         aLE ((dx, dy):stk) ((Id "in",(x,y)):ts) = (Id "in",(x,y)) : aLE stk ts
         aLE ((dx, dy):stk) ((t,(x,y)):ts) 
             | x <= dx && y>dy =  (EndOf LetLine, (x,y)) : (t,(x,y)) : aLE ( (x, y):stk) ts
             | otherwise = (t,(x,y)) : aLE ((dx, dy):stk) ts

addCaseEnds :: [(Tok,Pos)] -> [(Tok, Pos)] -- FIXME: nested
addCaseEnds = aCE []             
   where aCE _ [] = []
         aCE stk ((Id "of",(x',y')):(t,(x,y)):ts) = (Id "of",(x',y')) : (t,(x,y)) : aCE ((x,y):stk) ts
         aCE [] ((t,(x,y)):ts) = (t,(x,y)) : aCE [] ts
         aCE ((dx, dy):stk) ((t,(x,y)):ts) 
             | x == dx && y>dy =  (EndOf CaseLine, (x,y)) : (t,(x,y)) : aCE ( (x, y):stk) ts
             | x < dx && y>dy =   (t,(x,y)) : aCE ( stk) ts
             | otherwise = (t,(x,y)) : aCE ((dx, dy):stk) ts

addSwitchEnds :: [(Tok,Pos)] -> [(Tok, Pos)] -- FIXME: nested
addSwitchEnds = aSE []             
   where aSE _ [] = []
         aSE stk ((Id "switch",(x',y')):(t,(x,y)):ts) = (Id "switch",(x',y')) : (t,(x,y)) : aSE ((x,y):stk) ts
         aSE [] ((t,(x,y)):ts) = (t,(x,y)) : aSE [] ts
         aSE ((dx, dy):stk) ((t,(x,y)):ts) 
             | x == dx && y>dy =  (EndOf SwitchLine, (x,y)) : (t,(x,y)) : aSE ( (x, y):stk) ts
             | x < dx && y>dy =  (t,(x,y)) : aSE ( stk) ts
             | otherwise = (t,(x,y)) : aSE ((dx, dy):stk) ts


withLayout :: [(Tok,Pos)] -> [Tok]
withLayout tps = map fst $ addSwitchEnds $ addCaseEnds $ addLetEnds $ addDeclEnds tps