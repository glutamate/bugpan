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
            "sscan" =: (Lam "f" . Lam "v0" . Lam "s" $
                        LetE [("sr", (Sig $ (Var "f") $> (SigVal (Var "s")) $> (SigVal $ SigDelay (Var "sr") (Var "v0"))))
                             ] $ Var "sr"),
            "integrate" =: ((Var "sscan" $> (Var "intStep") $> (0))),
            "intStep" =: (Lam "new" . Lam "old" $ (Var "old") + (Var "new")*(Var "dt")),
            "seconds" =: Sig 1, --dummy
            "dt" =: 1 --dummy
          ]

testProg  = [--"secsp1" =: ((Var "smap") $> (Var "incr") $> (Var "seconds")),
             --"aval" =: 5,
             --"secsp1d1" =: ((Var "smap") $> (Var "incr") $> (SigDelay (Var "seconds") (0))),
             "accum_secs_plus1" =: ((Var "sscan") $> (Var "add") $> 0 $> ((Var "smap") $> (Var "incr") $> (Var "seconds"))  ),
             --"accsecs" =: ((Var "sscan") $> (Var "add") $> 0 $> (Var "seconds")  ),
             "intsecs" =: ((Var "integrate" $> (Var "accum_secs_plus1")))
            ]

ppProg prg = forM_ prg $ \e -> case e of 
                                 Let n e-> putStrLn (n++" = " ++ pp e)
                                 

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
            ifM (inBoundVars nm)
                (return e)
                $ do defn <- lookUp nm
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

changeVar :: String -> String -> E-> E
changeVar n1 n2 e = mapE aux e
    where aux (Var n) | n == n1 = Var n2
                      | otherwise = Var n
          aux e = e

changeVars :: [(String,String)] -> E-> E
changeVars [] e = e
changeVars ((no,nn):ns) e = changeVar no nn (changeVars ns e)
                   

betaReduce :: E-> E
betaReduce e = let bce = betaContract e
               in if bce == e 
                     then e
                     else betaReduce bce


unDelays :: TravM ()
unDelays = mapD unDelays' 
    where unDelays' e@(SigDelay (Var _) _) = return e
          unDelays' tle = mapEM undel tle
          undel (SigDelay (Var nm) initE) = do
            --assumes nm not bound and inite has no bound vars. FIXME
            dnm <- genSym $ nm++"_delay"
            insertAtEnd [Let dnm (SigDelay (Var nm) initE)]
            return (Var dnm)
          undel e = return e
            
letFloating ::TravM ()
letFloating = mapD letFl
    where letFl (LetE ses er) = do
            nns <- mapM genSym (map fst ses)
            let nonns = zip (map fst ses) nns
            nes <- forM (zip ses nns) $ \((n,e), nn) -> do
                                  return $ Let nn (changeVars nonns e)
            insertBefore nes
            return (changeVars nonns er) 

          letFl e = return e
                
sigFloating :: TravM ()
sigFloating = mapD sigFl
    where sigFl (Sig se) = Sig `fmap` mapEM sigFloat se
          sigFl e = mapEM sigFloat e
          sigFloat (Sig se) = do
            ifM (hasBoundVars se)
                (error "not sure what to do with bound vars in sig floating")
                $ do sn <- genSym "sigfl"
                     insertBefore [Let sn (Sig se)]
                     return (Var sn)
          sigFloat e = return e


inBoundVars :: String -> TravM Bool
inBoundVars nm = (nm `elem`) `fmap` boundVars `fmap` get

stripSig (Sig se) = se
stripSig e = e

hasBoundVars :: E-> TravM Bool
hasBoundVars e = or `fmap` queryM hasBvars e
    where hasBvars (Var nm) = (:[]) `fmap` inBoundVars nm
          hasBvars e = return [] 
                   

 


hasSig :: E->TravM Bool
hasSig e = or `fmap` queryM hasSigAux e
    where hasSigAux :: E -> TravM [Bool]
          hasSigAux (Sig _) = return [True]
          hasSigAux (Var nm) = ifM (inBoundVars nm)
                                   (return [False])
                                   $ do defn <- stripSig `fmap` lookUp nm
                                        queryM hasSigAux defn
          hasSigAux (_) = return [False] 
 



test = do putStrLn "prelude"
          ppProg prelude
          putStrLn "\ninitial"
          ppProg testProg
          putStrLn "\ntransformed"
          ppProg (snd . runTM $ do whileChanges substHasSigs  
                                   betaRedHasSigs 
                                   letFloating 
                                   sigFloating 
                                   unDelays
                 )

          --return $ hasSigProg testProg

--process :: E-> TravM Process

