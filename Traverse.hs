{- needed? LANGUAGE FlexibleInstances -}
module Traverse where

import Expr
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.List as L
import Debug.Trace
import Data.Maybe
import PrettyPrint
import TNUtils
import EvalM (T)
import Control.Monad.Error

data TravS = TravS { counter :: Int,
                     decls :: [Declare],
                     env ::  [(String, E)],
                     boundVars :: [String],
                     lineNum :: Int,
                     exprPath :: [E],
                     changed :: Bool,
                     tyConstraints :: [(T,T)]
                   }

type TravM a = StateT TravS (Either  String) a

alterDefinition :: String -> (E->E) -> TravM ()
alterDefinition nm f = do 
  ds <- decls `fmap` get
  let line = [ (def,lnum, t) | (Let (PatVar nm1 t) def ,lnum) <- zip ds [0..], nm==nm1]
  case line of
    (e, num, t):_ -> setter $ \s-> s { decls = setIdx num (Let (PatVar nm t) $ f e) ds }
    [] -> return ()

alterTypeDefinition :: String -> T -> TravM ()
alterTypeDefinition nm t = do 
  ds <- decls `fmap` get
  let line = [ (def,lnum) | (DeclareType nm1 def ,lnum) <- zip ds [0..], nm==nm1]
  case line of
    (e, num):_ -> setter $ \s-> s { decls = setIdx num (DeclareType nm t) ds }
    [] -> return ()

clearTyConstraints :: TravM ()
clearTyConstraints = setter (\s-> s {tyConstraints = []})

addTyConstraint :: (T,T) -> TravM ()
addTyConstraint con = setter (\s-> s {tyConstraints = con:tyConstraints s})
                      
traceDecls :: TravM ()
traceDecls = do ds <- decls `fmap` get
                mapM_ (\d->traceM $ ppDecl d) ds
traceDefn :: String -> TravM ()
traceDefn nm = do ds <- decls `fmap` get
                  mapM_ (\d->traceM $ ppDecl d) [ d | d@(Let (PatVar nm1 _) e) <- ds, nm1 == nm]

traceTyConstraints :: TravM ()
traceTyConstraints = do tcs <- tyConstraints `fmap` get
                        mapM_ (\(t1,t2)->traceM $ ppType t1++" `U` "++ppType t2) tcs

traceConstraints :: [(T,T)] -> TravM ()
traceConstraints = mapM_ (\(t1,t2)->traceM $ ppType t1++" `U` "++ppType t2)


setter :: (TravS -> TravS) -> TravM ()
setter f = do s <- get
              put $ f s

setIdx :: Int -> a-> [a]->[a]
setIdx _ _ [] = []
setIdx 0 y (x:xs) = y:xs
setIdx n y (x:xs) = x:setIdx (n-1) y xs

spliceAt :: Int -> [a]-> [a]->[a]
spliceAt n ys xs = let (hd, tl) = splitAt n xs in
                   concat [hd, ys, tl]

errorInfo :: String -> TravM a -> TravM a
errorInfo msg ma = ma `catchError` (\s-> throwError $ s++"\n"++msg)

runTravM :: [Declare] -> [(String, E)] -> TravM a -> (a, [Declare])
runTravM decs env tx 
    = let initS = TravS 0 decs env [] 0 [] False []
          (x, TravS _ decsFinal _ _ _ _ _ _) = case  runStateT tx initS of
                                                 Left s -> error $ "runTravM fail: "++s
                                                 Right tup -> tup
                                               -- Just x -> x
                                               -- Nothing -> error "runTravM returns mzero"
                                             
      in (x, decsFinal)



mapDE :: (E-> TravM E) -> TravM ()
mapDE f =do ds <- decls `fmap` get
            -- let lns = length ds
            setter $ \s-> s { lineNum = 0 }
            untilM (stopCond `fmap` get) $ do
              ln <- curLine
              --trace ("now doing line "++show ln) $ return ()
              case ln of 
                Let n e -> do 
                        e' <- errorInfo ("in line: "++ppDecl ln) $ f e
                        when (e' /= e) $ do markChange
                                            --trace (pp e++ " /= \n" ++ pp e') $ return ()
                                            lnum' <- lineNum `fmap` get -- f may insert lines above
                                            setter $ \s-> s { decls = setIdx lnum' (Let n e') (decls s)}
                _ -> return ()
              lnum <- lineNum `fmap` get
              setter $ \s-> s { lineNum = lnum+1 }
                     
stopCond :: TravS -> Bool
stopCond s = lineNum s >= length ( decls s)

           -- forM_ [0..(lns-1)] $ \lnum-> do 

mapD :: (Declare -> TravM Declare)  -> TravM ()
mapD f = do ds <- decls `fmap` get
            -- let lns = length ds
            setter $ \s-> s { lineNum = 0 }
            untilM (stopCond `fmap` get) $ do
              ln <- curLine
              ln' <- errorInfo ("in line: "++ppDecl ln) $ f ln
              when (ln /= ln') $ do markChange
                                    lnum' <- lineNum `fmap` get -- f may insert lines above
                                    setter $ \s-> s { decls = setIdx lnum' (ln') (decls s)}
              lnum <- lineNum `fmap` get
              setter $ \s-> s { lineNum = lnum+1 }


renameEverywhere :: String -> String -> TravM ()
renameEverywhere oldn newn 
    = do ds <- decls `fmap` get
         setter $ \s-> s { decls = map rnm ds}
    where rnm (Let (PatVar nm t) e) | nm == oldn = Let (PatVar newn t) $ mapE rne e
                                    | otherwise =  Let (PatVar nm t)  $ mapE rne e
          rnm (SinkConnect e nm) = SinkConnect (mapE rne e) nm
          rnm (Stage nm s) | nm == oldn = Stage newn s
                           | otherwise = Stage nm s
          rnm d = d
          
          rne (Var n) | n == oldn = Var newn
                      | otherwise = Var n
          rne e = e

curLine :: TravM Declare
curLine = do lnum <- lineNum `fmap` get
             (!!lnum) `fmap` decls `fmap` get

whileChanges :: TravM () -> TravM ()
whileChanges ma = do eraseChange
                     ma
                     whenM (changed `fmap` get)
                           (whileChanges ma) 

markChange = setter $ \s-> s { changed = True }
eraseChange = setter $ \s-> s { changed = False }


genSym :: String -> TravM String
genSym base = do tok <- counter `fmap` get
                 setter $ \s->s {counter = tok+1}
                 return $ base ++ "_" ++ (show tok)

withEnv :: String -> E -> TravM a -> TravM a
withEnv n e tx = do env1 <- env `fmap` get
                    setter $ \s-> s { env = (n,e):env1 }
                    x <- tx
                    setter $ \s-> s { env = env1 }
                    return x

withBvars n tx = do bvs <- boundVars `fmap` get
                    setter $ \s-> s { boundVars = n++bvs }
                    x <- tx
                    setter $ \s-> s { boundVars = bvs }
                    return x

withPath :: E -> TravM a -> TravM a
withPath e tx = do p <- exprPath `fmap` get
                   setter $ \s-> s {exprPath = e:p}
                   x<- tx
                   setter $ \s-> s {exprPath = p}
                   return x

insideSwitch :: TravM Bool
insideSwitch  = (isSwitch . head . exprPath) `fmap` get
    where isSwitch (Switch _ _) = True
          isSwitch _ = False

insideSolveOde :: TravM Bool
insideSolveOde  = (isOde . head . exprPath) `fmap` get
    where isOde (SigFby _ _) = True
          isOde (SolveOde _) = True
          isOde _ = False


lookUp :: String -> TravM (E)
lookUp nm = do env <- env `fmap` get
               case L.lookup nm env of
                 Just e -> return e
                 Nothing -> lookUpInDecls
    where lookUpInDecls = do ds <- declsToEnv `fmap` decls `fmap` get
                             case L.lookup nm ds of
                               Just e -> return e
                               Nothing -> do ln <- curLine
                                             fail $ "lookUp: can't find "++nm++" in line: "++ ppDecl ln++"\nin env"++show (map fst ds)
                          

safeLookUp :: String -> TravM (Maybe E)
safeLookUp nm = do env <- env `fmap` get
                   case L.lookup nm env of
                     Just e -> return $ Just e
                     Nothing -> lookUpInDecls
    where lookUpInDecls = do ds <- declsToEnv `fmap` decls `fmap` get
                             return $ L.lookup nm ds
          
insertAtTop :: [Declare] -> TravM ()
insertAtTop [] = return ()
insertAtTop ds = markChange >> (setter $ \s-> let ln = lineNum s
                                                  ds' = ds ++ decls s
                                               in s { decls = ds', lineNum = ln+length ds })


insertAtEnd :: [Declare] -> TravM ()
insertAtEnd [] = return ()
insertAtEnd ds = markChange >> (setter $ \s -> s {decls = decls s ++ ds})

insertBefore :: [Declare] -> TravM ()
insertBefore [] = return ()
insertBefore ds = markChange >> (setter $ \s-> let ln = lineNum s
                                                   ds' = spliceAt ln ds $ decls s
                                               in s { decls = ds', lineNum = ln+length ds })
insertAfter :: [Declare] -> TravM ()
insertAfter [] = return ()
insertAfter ds = markChange >> (setter $ \s-> let ln = lineNum s
                                                  ds' = spliceAt (ln+1) ds $ decls s
                                              in s { decls = ds', lineNum = ln+length ds })

declsToEnv [] = []
declsToEnv (Let (PatVar n t) e:ds) = (n,e):declsToEnv ds
declsToEnv (_:ds) = declsToEnv ds 

concatM :: Monad m => [m [a]] -> m [a]
concatM [] = return []
concatM (mlst:mlsts) = do lst <- mlst
                          lsts <- concatM mlsts
                          return (lst++lsts)

queryM :: (E-> TravM [a]) -> E -> TravM [a]
queryM q e = queryM' e
    where m = queryM q
          queryM' e@(If p c a) = concatM [q e,m p, m c,m a]
          queryM' e@(LetE ses er) = withBvars (concatMap (patIntroducedVars . fst) ses) $
                           concatM [q e, 
                                    concat `fmap` mapM m (map snd ses), 
                                    m er]
          queryM' e@(Switch ses er) = concatM [q e, 
                                      concat `fmap` mapM m (map fst ses), 
                                      concat `fmap` mapM m (map snd ses), 
                                      m er]
          queryM' e@(Lam n t bd) = withBvars [n] $ concatM [q e, m bd]
          queryM' e@(App le ae) = concatM [q e, m le, m ae]
          queryM' e@(Pair e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Cons e1 e2) =concatM [ q e, m e1, m e2]
          queryM' e@(M1 _ e1) = concatM [q e, m e1]
          queryM' e@(M2 _ e1 e2) = concatM [q e, m e1, m e2] 
          queryM' e@(Cmp _ e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(And e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Or e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Not e1) = concatM [q e, m e1]
          queryM' e@(Sig e1) = concatM [q e, m e1]
          queryM' e@(SigVal e1) = concatM [q e, m e1]
          queryM' e@(SolveOde e1) = concatM [q e, m e1]
          queryM' e@(SigDelay e1 e2) = concatM [q e, m e1,  m e2]
          queryM' e@(SigFby e1 e2) = concatM [q e, m e1,  m e2]
          queryM' e@(SigLimited e1 e2) = concatM [q e, m e1,  m e2]
          queryM' e@(ETest e1 e2) = concatM [q e, m e1,  m e2]
          queryM' e@(EScan e1 e2) = concatM [q e, m e1,  m e2]
          queryM' e@(Event e1) = concatM [q e, m e1]
          queryM' e@(Forget e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Const _) = concatM [q e]
          queryM' e@(SigAt e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Var _) = concatM [q e]
          queryM' e@(Nil) = concatM [q e]
          queryM' e@(Box e1) = concatM [q e, m e1]
          queryM' e@(Translate e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(Colour e1 e2) = concatM [q e, m e1, m e2]
          queryM' e@(HasType _ e1) = concatM [q e, m e1]
          queryM' e@(Case ce cs) = concatM [q e, q ce, 
                                            concat `fmap` mapM (\(p,ep)->  
                                                                withBvars (patIntroducedVars p) 
                                                                          (m ep)) cs]

--queryM q e = fail $ "queryM: unknown expr "++show  e 


mapEM :: (E-> TravM E)-> E -> TravM E
mapEM f e = mapEM' e
    where m = withPath e . mapEM f 
          mapEM' (If p c a) = ( return If `ap` m p `ap` m c `ap` m a) >>= f
          mapEM' (Lam n t bd) = (withBvars [n] $ return (Lam n t) `ap` m bd) >>= f
          mapEM' (App le ae) = (return App `ap` m le `ap` m ae) >>= f
          mapEM' (Var n) = f $ Var n
          mapEM' (Sig s) = (return Sig `ap` m s) >>= f
          mapEM' (SigVal s) = (return SigVal `ap` m s) >>= f
          mapEM' (SolveOde s) = (return SolveOde `ap` m s) >>= f
          mapEM' (SigDelay s1 s2) = (return SigDelay `ap` m s1 `ap` m s2) >>= f
          mapEM' (SigFby s1 s2) = (return SigFby `ap` m s1 `ap` m s2) >>= f
          mapEM' (SigLimited s1 s2) = (return SigLimited `ap` m s1 `ap` m s2) >>= f
          mapEM' (SigAt s1 s2) = (return SigAt `ap` m s1 `ap` m s2) >>= f
          mapEM' (M1 op s) = (return (M1 op) `ap` m s) >>= f
          mapEM' (M2 op s1 s2) = (return (M2 op) `ap` m s1 `ap` m s2 ) >>= f
          mapEM' (And s1 s2) = (return And `ap` m s1 `ap` m s2) >>= f
          mapEM' (Or s1 s2) = (return Or `ap` m s1 `ap` m s2) >>= f
          mapEM' (Cons s1 s2) = (return Cons `ap` m s1 `ap` m s2) >>= f
          mapEM' (Not s) = (return Not `ap` m s) >>= f
          mapEM' (Cmp o s1 s2) = (return (Cmp o) `ap` m s1 `ap` m s2) >>= f
          mapEM' (Pair s1 s2) = (return Pair `ap` m s1 `ap` m s2) >>= f
          mapEM' (Event s2) = (return Event `ap` m s2) >>= f
          mapEM' (ETest s1 s2) = (return ETest `ap` m s1 `ap` m s2) >>= f
          mapEM' (EScan s1 s2) = (return EScan `ap` m s1 `ap` m s2) >>= f
          mapEM' (Forget s1 s2) = (return Forget `ap` m s1 `ap` m s2) >>= f
          mapEM' (Const c) = f $ Const c
          mapEM' (Nil) = (f Nil)

          mapEM' (Box s1) = (return Box `ap` m s1) >>= f
          mapEM' (Translate s1 s2) = (return Translate `ap` m s1 `ap` m s2) >>= f
          mapEM' (Colour s1 s2) = (return Colour `ap` m s1 `ap` m s2) >>= f
          mapEM' (HasType t s2) = (return (HasType t) `ap` m s2) >>= f

          mapEM' (LetE ses er) = (
              withBvars (concatMap (patIntroducedVars . fst) ses) $ 
              return LetE `ap` mapM (\(p,e)-> return ((,) p) `ap` m e) ses 
                          `ap` m er) >>= f
          mapEM' (Case e pats) = (return Case `ap` 
                                         m e `ap` 
                                         mapM (\(pat, ep)-> 
                                               (pair pat) `fmap` (withBvars (patIntroducedVars pat) $ m ep)) pats 
                          ) >>= f
          mapEM' (Switch ses er) = (
              return Switch `ap` mapM (\(e1,e2)-> return (,) `ap` m e1 `ap` m e2) ses 
                            `ap` m er) >>= f
          pair x y = (x,y)
          -- mapE f e = error $ "mapE: unknown expr "++show e 
              
