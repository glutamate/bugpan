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
   print $ parseDs testBug
   mapM (runTst $ parseE stdFixity) etsts
   mapM (runTst parsePat) pattsts
   mapM (runTst parseTy) tytsts
   mapM (runTst $ parseD stdFixity) dtsts
 
infixl 1 #
(#) = (,)

--runTst :: (String ,E) -> IO ()
runTst the_parser (s, e) 
    = do           let toks = map fst $lex 0 0 s
                   --print toks
                   case parse the_parser "" toks of
                     Left err -> putStrLn $ "error in "++s++ show err
                     Right x | x == e -> return () -- putStrLn $ "pass: "++s
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

pattsts :: [(String, Pat)]
pattsts = [
  "x" # PVar "x"
 ,"_" # PWild
 ,"1" # PLit (VInt 1)
          ]

tytsts :: [(String, T)]
tytsts = [
    "a" # TVar "a"
   ,"A" # TCon "A"
   ,"a->A" # TLam (TVar "a") (TCon "A") 
   ,"a->b->c" # TLam (TVar "a") (TLam (TVar "b") (TVar "c"))  
   ,"(a->b)->c" # TLam (TLam (TVar "a") (TVar "b")) (TVar "c") 
   ,"a->(b->c)" # TLam (TVar "a") (TLam (TVar "b") (TVar "c")) 
   ,"(a->b) -> Signal a -> Signal b" # 
      TLam (TLam (TVar "a") (TVar "b")) (TLam (TApp (TCon "Signal") (TVar "a")) (TApp (TCon "Signal") (TVar "b")))
  ]

dtsts :: [(String, D)]
dtsts = [
    "import Foo" # DImport "Foo"
   ,"f x = 2*x" # DLet [PVar "f", PVar "x"] (2*EVar "x")
   ,"data Bool = T | F" # DMkType "Bool" [] [("T", []),("F", [])]
   ,"data Maybe ɑ = Just ɑ | Nothing" # DMkType "Maybe" ["ɑ"] [("Just",[TVar "ɑ"]),("Nothing",[])]
  ]
