{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Baysig.Lexer where

import Prelude hiding (lex)
import Data.List
import TNUtils
import Data.Generics


data Parentheticals = Parens
                    | Sig
                    | SigVal
                    | Curly
          deriving (Show, Eq, Read, Data, Typeable)              

data Tok = Id String
         | Op String
         | Open Parentheticals
         | Close Parentheticals
         | TokInt Int
         | TokFloat Double
         | Eos
         | Underscore
         deriving (Show, Eq, Read, Data, Typeable)

opChars = ".:^*+-=<>&%$!#%|/\\"
wsChars = " \t\r\n"
eolChars = "\r\n"
initIdChars = ['A'..'Z']++['a'..'z']++['_']++['ɑ'..'ω']
idChars = initIdChars ++ ['0'..'9']++ ['\'']
digits = ['0'..'9']
numChar = digits ++ ['.', 'e', '-']


lex :: String -> [Tok]
lex [] = []
lex ('-':'-':cs) = lex $ dropWhile (`notElem` eolChars) cs
lex ('{':'-':cs) = lex $ dropComment cs
lex (';':cs) = Eos : lex cs
lex ('(':cs) = Open Parens : lex cs
lex (')':cs) = Close Parens : lex cs
lex ('{':':':cs) = Open Sig : lex cs
lex (':':'}':cs) = Close Sig : lex cs
lex ('{':cs) = Open Curly : lex cs
lex ('}':cs) = Close Curly : lex cs
lex ('<':':':cs) = Open SigVal : lex cs
lex (':':'>':cs) = Close SigVal : lex cs
lex inp@(c:cs) 
   | isOp && "{:" `isSuffixOf` opNm = Op (dropEnd 2 opNm) : lex ("{:"++rest)
   | isOp && "<:" `isSuffixOf` opNm = Op (dropEnd 2 opNm) : lex ("<:"++rest)
   | isOp = Op opNm : lex rest
   | c `elem` wsChars = lex cs
   | c == '_' && (null cs || (head cs `notElem` idChars)) = Underscore : lex cs
   | c `elem` initIdChars = let (idNm, rest) = span (`elem` idChars) inp
                            in Id idNm : lex rest
   | c `elem` digits = let (numStr, rest) = takeNumStr inp in
                       case safeRead numStr of
                         Just n -> TokInt n : lex rest
                         Nothing -> case safeRead numStr of
                                        Just n -> TokFloat n : lex rest
                                        _ -> error $ "lex: the impossible happened!"
   | "infixl" `isPrefixOf` inp = lex $ dropWhile (`notElem` eolChars) cs
   | "infixr" `isPrefixOf` inp = lex $ dropWhile (`notElem` eolChars) cs
   | otherwise = error $ "lex: unknown character "++[c]++" in "++take 10 inp
    where (opNm, rest) = span (`elem` opChars) inp
          isOp = c `elem` opChars 

dropComment [] = []
dropComment ('-':'}':cs) = cs
dropComment (_:cs) = dropComment cs

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

--x `notElem` xs = not $ x `elem` xs