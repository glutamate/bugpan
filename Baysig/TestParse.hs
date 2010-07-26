module Main where

import Baysig.Expr
import Baysig.Lexer
import Prelude hiding (lex)

main = do 
   testBug <- readFile "Test.bug"
   mapM print $ lex testBug
   print $ 1+(2::E)

etsts :: [(String, E)]
etsts = [
      "exp 4 + 3" # (4+3)
      ]
      where (#) = (,)