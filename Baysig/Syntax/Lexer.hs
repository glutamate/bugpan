{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Syntax.Lexer where

import Prelude hiding (lex)
import Data.List
import TNUtils
import Data.Generics

data Endable = Declaration | LetLine | CaseLine | SwitchLine
          deriving (Show, Eq, Read, Data, Typeable)              

data Parentheticals = Parens | Sig | SigVal | Curly
          deriving (Show, Eq, Read, Data, Typeable)              

data Tok = Id String 
         | Op String 
         | Open Parentheticals 
         | Close Parentheticals 
         | TokInt Int 
         | TokFloat Double 
         | EndOf Endable 
         | Underscore 
         deriving (Show, Eq, Read, Data, Typeable)

opChars = ".:^*+-=,<>~&%$!#%|/\\"
wsChars = " \t"
eolChars = "\r\n"
initIdChars = ['A'..'Z']++['a'..'z']++['_']++['ɑ'..'ω']
idChars = initIdChars ++ ['0'..'9']++ ['\'']
digits = ['0'..'9']
numChar = digits ++ ['.', 'e', '-']

type Pos = (Int, Int)

at x y t = (t, (x, y))

lex :: Int -> Int -> String -> [(Tok,Pos)]
lex x y [] = []
lex x y ('-':'-':cs) = lex x y $ dropWhile (`notElem` eolChars) cs
lex x y ('{':'-':cs) = lexComment (x+2) y cs
lex x y ('(':cs) = at x y (Open Parens) : lex (x+1) y cs
lex x y (')':cs) = at x y (Close Parens) : lex (x+1) y cs
lex x y ('{':':':cs) = at x y (Open Sig) : lex (x+2) y cs
lex x y (':':'}':cs) = at x y (Close Sig) : lex (x+2) y cs
lex x y ('{':cs) = at x y (Open Curly) : lex (x+1) y cs
lex x y ('}':cs) = at x y (Close Curly) : lex (x+1) y cs
lex x y ('<':':':cs) = at x y (Open SigVal) : lex (x+2) y cs
lex x y (':':'>':cs) = at x y (Close SigVal) : lex (x+2) y cs
lex x y inp@(c:cs) 
   | isOp && "{:" `isSuffixOf` opNm = let opNm' = dropEnd 2 opNm
                                      in at x y (Op opNm') : lex (x+length opNm') y ("{:"++rest)
   | isOp && "<:" `isSuffixOf` opNm = let opNm' = dropEnd 2 opNm
                                      in at x y (Op opNm') : lex (x+length opNm') y ("<:"++rest)
   | isOp = at x y (Op opNm) : lex (x+length opNm) y rest
   | c `elem` wsChars = lex (x+1) y cs
   | c `elem` eolChars = lex 0 (y+1) cs
   | c == '_' && (null cs || (head cs `notElem` idChars)) = at x y (Underscore) : lex (x+1) y cs
   | any (`isPrefixOf` inp) $ map (++" ") $ words "infixr infixl infix prefix postfix"
       = lex x y $ dropWhile (`notElem` eolChars) cs
   | c `elem` initIdChars = let (idNm, rest) = span (`elem` idChars) inp
                            in at x y (Id idNm) : lex (x+length idNm) y rest
   | c `elem` digits = let (numStr, rest) = takeNumStr inp in
                       case safeRead numStr of
                         Just n -> at x y (TokInt n) : lex (x+length numStr) y rest
                         Nothing -> case safeRead numStr of
                                        Just n -> at x y (TokFloat n) : lex (x+length numStr) y rest
                                        _ -> error $ "lex: the impossible happened!"
   | otherwise = error $ "lex: unknown character "++[c]++" in "++take 10 inp
    where (opNm, rest) = span (`elem` opChars) inp
          isOp = c `elem` opChars 

lexComment x y [] = []
lexComment x y ('-':'}':cs) = lex (x+2) y cs
lexComment x y (c:cs) | c `elem` eolChars = lexComment 0 (y+1) cs
                      | otherwise = lexComment (x+1) y cs

dropEnd n xs = take (max 0 $ length xs - n) xs


takeNumStr :: String -> (String, String)
takeNumStr inp = tak inp []
   where tak [] acc = (reverse acc, [])
         tak ('.':cs) acc | '.' `notElem` acc && 
                            'e' `notElem` acc = tak cs ('.':acc)
                          | otherwise = (reverse acc, '.':cs)
         tak ('e':cs) acc | 'e' `notElem` acc = tak cs ('e':acc)
                          | otherwise = (reverse acc, 'e':cs)
         tak ('-':cs) acc | 'e' `elem` acc = tak cs ('-':acc)
                          | otherwise = (reverse acc, '-':cs)                        
         tak (c:cs) acc   | c `elem` digits = tak cs (c:acc)
                          | otherwise = (reverse acc, c:cs)

