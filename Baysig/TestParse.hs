module Main where

import Baysig.Expr
import Baysig.Syntax.Parser
import Baysig.Syntax.Layout
import Baysig.Syntax.Fixity
import Text.Parsec.String 
import Text.Parsec
import Baysig.Transform
main = do 
   tstParseFile "Test.bug"
   tstParseFile "Nats.bug"
   mapM (runTst $ parseE stdFixity) etsts
   mapM (runTst $ parsePat stdFixity) pattsts
   mapM (runTst parseTy) tytsts
   mapM (runTstD) dtsts
 
infixl 1 #
(#) = (,)

quiet = True

runTstD :: (String, D) -> IO ()
runTstD (s, e) 
    = do           let toks = lexWithLayout s
                   --print toks
                   case (head . removeTopLevelPatterns . (:[])) `fmap` parse (parseD stdFixity) "" toks of
                     Left err -> do putStrLn $ "error in "++s++ show err
                                    putStrLn $ "tokens: "++show toks
                     Right x | x == e -> if quiet then return () else putStrLn $ "pass: "++s
                             | otherwise -> do putStrLn $ "not the same: "++s++" \ngot : "++show x
                                               putStrLn $ "expected: "++show e
                                               putStrLn $ "tokens: "++show toks

--runTst :: (String ,E) -> IO ()
runTst the_parser (s, e) 
    = do           let toks = lexWithLayout s
                   --print toks
                   case parse the_parser "" toks of
                     Left err -> do putStrLn $ "error in "++s++ show err
                                    putStrLn $ "tokens: "++show toks
                     Right x | x == e -> if quiet then return () else putStrLn $ "pass: "++s
                             | otherwise -> do putStrLn $ "not the same: "++s++" \ngot : "++show x
                                               putStrLn $ "expected: "++show e
                                               putStrLn $ "tokens: "++show toks

tstParseFile nm =  do
   testBug <- readFile nm
   --mapM print $ map fst $ addDeclEnds $ lex 0 0 testBug
   case parseDs testBug of
     Left err -> do putStrLn $ "parse error in "++nm++": "++ err
                    mapM_ print $ lexWithLayout testBug
     Right ds -> if quiet then return () else putStrLn $ nm++" parse Ok"


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
      "\\_->2*1" # ELam (PWild) (2*1),
      "\\x y->y*x" # ELam (PVar "x") (ELam (PVar "y") (EVar "y"*EVar "x")),
      "(\\x->2*x) 4" # EApp (ELam (PVar "x") (2*EVar "x")) 4
     ,"{: 1 :}" # EApp (EVar "sig") 1
     ,"<: s :>" # EApp (EVar "sigval") (EVar "s")
     ,"(x:A)" # ETy (TCon "A") (EVar "x")
     ,"()" # ECon (VCons "()" [])
     ,"let x = 5 in x+2" # ELet [(PVar "x", 5)] (EVar "x"+2)
     ,"if p then c else a" # EVar "if" $> EVar "p" $> EVar "c" $>EVar "a"
     ,"case x of e->5" # ECase (EVar "x") [(PVar "e",5)]
     ,"switch e ~> z" #  ECase (EVar "_switch") [(PVar "e",EVar "z")]
       ]

pattsts :: [(String, Pat)]
pattsts = [
  "x" # PVar "x"
 ,"_" # PWild
 ,"1" # PLit (VInt 1)
 ,"u!" # PBang (PVar "u")
 ,"Z" # PCons "Z" []
 ,"(P x y)" # PCons "P" [PVar "x",PVar "y"]
 ,"(S n)" # PCons "S" [PVar "n"]
          ]

tytsts :: [(String, T)]
tytsts = [
    "a" # TVar "a"
   ,"A" # TCon "A"
   ,"a->A" # TLam (TVar "a") (TCon "A") 
   ,"a->b->c" # TLam (TVar "a") (TLam (TVar "b") (TVar "c"))  
   ,"(a->b)->c" # TLam (TLam (TVar "a") (TVar "b")) (TVar "c") 
   ,"a->(b->c)" # TLam (TVar "a") (TLam (TVar "b") (TVar "c")) 
   ,"(a->b)-> Signal a -> Signal b" # 
      TLam (TLam (TVar "a") (TVar "b")) (TLam (TApp (TCon "Signal") (TVar "a")) (TApp (TCon "Signal") (TVar "b")))
  ]

dtsts :: [(String, D)]
dtsts = [
    "import Foo" # DImport "Foo"
   ,"f x = 2*x" # DLet [PVar "f", PVar "x"] (2*EVar "x")
   ,"data Bool = T | F" # DMkType "Bool" [] [("T", []),("F", [])]
   ,"data Maybe ɑ = Just ɑ | Nothing" # DMkType "Maybe" ["ɑ"] [("Just",[TVar "ɑ"]),("Nothing",[])]
   ,"myfun : a -> b -> c" # DDecTy ["myfun"] (TLam (TVar "a") (TLam (TVar "b") (TVar "c")))
   ,"myfun, otherfun : a -> b -> c" # DDecTy ["myfun", "otherfun"] (TLam (TVar "a") (TLam (TVar "b") (TVar "c")))
   ,"xyz *> dac 1" # DSink (EVar "xyz") "dac" 1
   ,"xyz <* adc 1" # DSource (PVar "xyz") "adc" 1
   ,"plus Z m = m" # DLet [PVar "plus", PCons "Z" [], PVar "m"] (EVar "m")
   ,"plus (S p) m = p" # DLet [PVar "plus", PCons "S" [PVar "p"], PVar "m"] (EVar "p")
   ,"(S p) + m = p" # DLet [PVar "+", PCons "S" [PVar "p"], PVar "m"] (EVar "p")
--   ,"S p + m = p" # DLet [PVar "+", PCons "S" [PVar "p"], PVar "m"] (EVar "p")
   ,"incr n = S n" # DLet [PVar "incr", PVar "n"] (EVar "S" $> EVar "n")
   ,"+ : Nat -> Nat -> Nat" # DDecTy ["+"] (TLam (TCon "Nat") (TLam (TCon "Nat") (TCon "Nat")))
  ]
 