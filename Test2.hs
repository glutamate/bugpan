module Test2 where

import Expr
import Eval
import EvalM
import Run
import Control.Monad
import Numbers
import Data.Char
import Traverse
import Control.Monad.State.Strict


type Program = [Declare]

prelude = [ "smap" =: (Lam "f" . Lam "s" $ Sig (Var "f" $> (SigVal $ Var "s"))),
            "incr" =: (Lam "x" (Var "x" + 1)),
            "seconds" =: Sig 1
          ]

testProg  = ["secsp1" =: ((Var "smap") $> (Var "incr") $> (Var "seconds")),
             "aval" =: 5]

ppProg prg = forM_ prg $ \(Let n e)-> putStrLn (n++" := " ++ pp e)

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst $ runTravM p (declsToEnv prelude) $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

run = runTravM testProg (declsToEnv prelude)

infixl 1 =:                                         
x =: y = Let x y 


hasSigAux :: E -> TravM [Bool]
hasSigAux (Sig _) = return [True]
hasSigAux (Var nm) = do bvs <- boundVars `fmap` get
                        if nm `elem` bvs
                            then return [False]
                            else do defn <- lookUp nm
                                    case defn of
                                      Sig se -> queryM hasSigAux se
                                      _ -> queryM hasSigAux defn
                                    --return $ or bs
                     
hasSigAux (_) = return [False]

hasSig :: E->TravM Bool
hasSig e = do or `fmap` queryM hasSigAux e

test = do ppProg testProg
          return $ hasSigProg testProg

--process :: E-> TravM Process