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
import Debug.Trace

type Program = [Declare]

prelude = [ "smap" =: (Lam "f" . Lam "s" $ Sig (Var "f" $> (SigVal $ Var "s"))),
            "incr" =: (Lam "x" (Var "x" + 1)),
             "add" =: (Lam "x" $ Lam "y" $ Var "x" + Var "y"),
            "seconds" =: Sig 1,
            "sscan" =: (Lam "f" . Lam "v0" . Lam "s" $
                        LetE [("sr", (Sig $ (Var "f") $> (SigVal (Var "s")) $> (SigVal $ SigDelay (Var "sr") (Var "v0"))))
                             ] $ Var "sr")
 
          ]

testProg  = ["secsp1" =: ((Var "smap") $> (Var "incr") $> (Var "seconds")),
             "aval" =: 5,
             "secsp1d1" =: ((Var "smap") $> (Var "incr") $> (SigDelay (Var "seconds") (0))),
             "accsecs" =: ((Var "sscan") $> (Var "add") $> 0 $> (Var "seconds")  )
            ]

ppProg prg = forM_ prg $ \e -> case e of 
                                 Let n e-> putStrLn (n++" = " ++ pp e)
                                 InitVal n e -> putStrLn (n++"(0) = " ++ pp e)

hasSigProg :: Program -> [(String, Bool)]
hasSigProg p = fst . runTM $ 
               forM p $ \(Let n e)-> do hs <- hasSig e
                                        return (n, hs)

runTM = runTravM testProg (declsToEnv prelude)

infixl 1 =:                                         
x =: y = Let x y 

substHasSigs :: TravM ()
substHasSigs = mapD $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            defn <- lookUp nm
            ifM (hasSig $ stripSig defn)
                (return $ App (defn) arg)
                (return e) 
          subst e = return e

betaRedHasSigs :: TravM ()
betaRedHasSigs = mapD $ \tle -> mapEM brhs tle
    where brhs e = ifM (hasSig e)
                       (return . betaContract $ e)
                       (return e)


betaContract :: E -> E
betaContract (App (Lam nm bd) arg) = mapE bar bd
    where bar e@(Var n') | n' == nm = arg -- doesnt work if shadows
                         | otherwise = e
          bar e = e
betaContract e = e

changeVars :: String -> String -> E-> E
changeVars n1 n2 e = mapE aux e
    where aux (Var n) | n == n1 = Var n2
                      | otherwise = Var n
          aux e = e
                     

betaReduce :: E-> E
betaReduce e = let bce = betaContract e
               in if bce == e 
                     then e
                     else betaReduce bce


unDelays :: TravM ()
unDelays = mapD $ \tle -> mapEM undel tle
    where undel (SigDelay (Var nm) initE) = do
            --assumes nm not bound and inite has no bound vars. FIXME
            dnm <- genSym $ nm++"_delay"
            insertAtEnd [Let dnm (SigDelay (Var nm) initE)]
            return (Var dnm)
          undel e = return e
            
letFloating ::TravM ()
letFloating = mapD letFl
    where letFl (LetE ses er) = do
            nes <- forM ses $ \(n,e) -> do
                                  nn <- genSym n
                                  return $ Let nn e 
            insertBefore $ nes
            return er
          letFl e = return e
                

inBoundVars :: String -> TravM Bool
inBoundVars nm = (nm `elem`) `fmap` boundVars `fmap` get

stripSig (Sig se) = se
stripSig e = e

hasSigAux :: E -> TravM [Bool]
hasSigAux (Sig _) = return [True]
hasSigAux (Var nm) = ifM (inBoundVars nm)
                            (return [False])
                            $ do defn <- stripSig `fmap` lookUp nm
                                 queryM hasSigAux defn
                        --return $ or bs
                     
hasSigAux (_) = return [False]

hasSig :: E->TravM Bool
hasSig e = do or `fmap` queryM hasSigAux e

test = do putStrLn "prelude"
          ppProg prelude
          putStrLn "\ninitial"
          ppProg testProg
          putStrLn "\ntransformed"
          ppProg (snd . runTM $ substHasSigs >> betaRedHasSigs)-- >> unDelays)

          --return $ hasSigProg testProg

--process :: E-> TravM Process

ifM :: Monad m => m Bool -> m a ->  m a -> m a
ifM mp mc ma = do p <- mp
                  if p
                     then mc
                     else ma
