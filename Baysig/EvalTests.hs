module Main where

import Baysig.Expr
import Baysig.Eval
import Baysig.Syntax.Parser
import Text.Parsec 
import Baysig.Syntax.Fixity
import Baysig.Syntax.Layout
import Baysig.RunModule
import Baysig.Transform

tstNats = do
   testBug <- readFile "Nats.bug"
   --mapM print $ map fst $ addDeclEnds $ lex 0 0 testBug
   ds <- case parseDs testBug of
           Left err -> fail $ "parse error in Nats.bug: "++ err 
           Right ds -> return ds
   --mapM_ print $ ds
   --mapM_ print $ removeTopLevelPatterns ds

   print $ runModule emptyEnv $ removeTopLevelPatterns ds

evtsts = [
  "(\\_->2) 3" # VInt 2
 ,"let x = 5 in x" # VInt 5
  ]
    


infixl 1 #
(#) = (,)

quiet = False

ioTst (s,v) = do
  case runTst (s,v) of
    Right _ -> return ()
    Left s -> putStrLn s

runTst (s,v) = do
  e <- showErr $ parse (parseE stdFixity) "" $ lexWithLayout s
  v' <- eval emptyEnv e
  if v' == v 
     then if quiet then Right () else Left $ "pass: "++s
     else Left $ "not the same: "++s++" => "++show v'++", expected "++show v

main = do
  mapM ioTst evtsts
  tstNats
  --print $ subVar "x" 5 $ ELam (PVar "x") $ EVar "x"