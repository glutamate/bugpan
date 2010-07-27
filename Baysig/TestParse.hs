module Main where

import Baysig.Expr
import Baysig.Lexer
import Baysig.Parser
import Prelude hiding (lex)
import Text.Parsec.String 
import Text.Parsec

main = do 
   testBug <- readFile "Test.bug"
   --mapM print $ lex testBug
   --print $ parseDs testBug
   mapM  runTst etsts
 
infixl 1 #
(#) = (,)

runTst :: (String ,E) -> IO ()
runTst (s, e) = do let toks = lex s
                   --print toks
                   case parse (parseE stdFixity) "" toks of
                     Left err -> putStrLn $ "error in "++s++ show err
                     Right x | x == e -> putStrLn $ "pass: "++s
                             | otherwise -> do putStrLn $ "not the same: "++s++" \ngot : "++show x
                                               putStrLn $ "expected: "++show e
                                               putStrLn $ "tokens: "++show toks

etsts :: [(String, E)]
etsts = [
      "2*2" # (2*2),
      "2*x" # (2*EVar "x"),
      "1+4*3" # 1+4*3,
      "1*4+3" # 1*4+3,
      "1*(8+5)+3" # 1*(8+5)+3,
      "exp 4+3" # exp 4+3,
      "log 4" # log 4,
      "2+exp 4+3" # 2+exp 4+3,
      "2*x" # (2*EVar "x"),
      "\\x->2*x" # ELam (PVar "x") (2*EVar "x"),
      "(\\x->2*x) 4" # EApp (ELam (PVar "x") (2*EVar "x")) 4
      ]