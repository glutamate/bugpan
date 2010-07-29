module Main where

import Baysig.Expr
import Baysig.Eval
import Baysig.Parser
import Text.Parsec 
import Baysig.Fixity
import Baysig.Lexer
import Baysig.Layout
import Prelude hiding (lex)


evtsts = [
 "(\\_->2) 3" # VInt 2
  ]
    


infixl 1 #
(#) = (,)

quiet = True

ioTst (s,v) = do
  case runTst (s,v) of
    Right _ -> return ()
    Left s -> putStrLn s

runTst (s,v) = do
  e <- showErr $ parse (parseE stdFixity) "" $ withLayout $ lex 0 0 s
  v' <- eval [] e
  if v' == v 
     then if quiet then Right () else Left $ "pass: "++s
     else Left $ "not the same: "++s++" => "++show v'++", expected "++show v

main = do
  mapM ioTst evtsts
  print $ subVar "x" 5 $ ELam (PVar "x") $ EVar "x"