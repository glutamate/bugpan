module Bugpan.Lexer where

import Prelude hiding (lex)
import Data.List

data Parentheticals = Parens
                    | Sig
                    | SigVal
                    | Curly

data Tok = Id String
         | Op String
         | Open Parentheticals
         | Close Parentheticals

opChars = ".:^*+-=<>&%$!#%|/\\"
wsChars = " \t\r\n"
initIdChars = ['A'..'Z']++['a'..'z']++['_']
idChars = initIdChars ++ ['0'..'9']++ ['\'']

lex :: String -> [Tok]
lex [] = []
lex ('(':cs) = Open Parens : lex cs
lex (')':cs) = Close Parens : lex cs
lex ('{':':':cs) = Open Sig : lex cs
lex (':':'}':cs) = Close Sig : lex cs
lex ('{':cs) = Open Curly : lex cs
lex ('}':cs) = Close Curly : lex cs
lex ('<':':':cs) = Open SigVal : lex cs
lex (':':'>':cs) = Close SigVal : lex cs
lex inp@(c:cs) | c `elem` opChars = let (opNm, rest) = span (`elem` opChars) cs 
                                    in Op opNm : lex rest
               | c `elem` wsChars = lex cs
               | c `elem` initIdChars = let (idNm, rest) = span (`elem` idChars) cs 
                                        in Id idNm : lex rest
               | otherwise = error $ "lex: unknown character "++[c]++" in "++take 10 inp