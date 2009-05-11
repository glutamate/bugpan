{- needed? LANGUAGE FlexibleInstances -}
module Traverse where

import Expr
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.List as L
import Debug.Trace
import Data.Maybe


data TravS = TravS { counter :: Int,
                     decls :: [Declare],
                     env ::  [(String, E)],
                     boundVars :: [String],
                     lineNum :: Int,
                     exprPath :: [E],
                     changed :: Bool
                   }

type TravM = StateT TravS Identity

guardM :: MonadPlus m => m Bool -> m ()
guardM mb = mb >>= guard

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

runTravM :: [Declare] -> [(String, E)] -> TravM a -> (a, [Declare])
runTravM decs env tx 
    = let initS = TravS 0 decs env [] 0 [] False
          (x, TravS _ decsFinal _ _ _ _ _) = runIdentity $ runStateT tx initS 
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
                        e' <- f e
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
              ln' <- f ln
              when (ln /= ln') $ do markChange
                                    lnum' <- lineNum `fmap` get -- f may insert lines above
                                    setter $ \s-> s { decls = setIdx lnum' (ln') (decls s)}
              lnum <- lineNum `fmap` get
              setter $ \s-> s { lineNum = lnum+1 }


renameEverywhere :: String -> String -> TravM ()
renameEverywhere oldn newn 
    = do ds <- decls `fmap` get
         setter $ \s-> s { decls = map rnm ds}
    where rnm (Let nm e) | nm == oldn = Let newn $ mapE rne e
                         | otherwise =  Let nm $ mapE rne e
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

whenM mp ma = do p<-mp
                 when p ma

untilM :: Monad m => m Bool -> m () -> m ()
untilM mp ma = do p <- mp
                  if p 
                     then return ()
                     else ma >> untilM mp ma
                     

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
declsToEnv ((Let n e):ds) = (n,e):declsToEnv ds
declsToEnv (_:ds) = declsToEnv ds 

concatM :: Monad m => [m [a]] -> m [a]
concatM [] = return []
concatM (mlst:mlsts) = do lst <- mlst
                          lsts <- concatM mlsts
                          return (lst++lsts)

queryM :: (E-> TravM [a]) -> E -> TravM [a]
queryM q e@(If p c a) = concatM [q e,m p, m c,m a]
	where m = queryM q

queryM q e@(LetE ses er) = withBvars (map fst ses) $
                           concatM [q e, 
                                    concat `fmap` mapM m (map snd ses), 
                                    m er]
    where m = queryM q
queryM q e@(Switch ses er) = concatM [q e, 
                                      concat `fmap` mapM m (map fst ses), 
                                      concat `fmap` mapM m (map snd ses), 
                                      m er]
    where m = queryM q
queryM q e@(Lam n bd) = withBvars [n] $ concatM [q e, m bd]
	where m = queryM q
queryM q e@(App le ae) = concatM [q e, m le, m ae]
	where m = queryM q
queryM q e@(Pair e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(Cons e1 e2) =concatM [ q e, m e1, m e2]
	where m = queryM q
queryM q e@(M1 _ e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(M2 _ e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(Cmp _ e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(And e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(Or e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(Not e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(Sig e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(SigVal e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(SigDelay e1 e2) = concatM [q e, m e1,  m e2]
	where m = queryM q
queryM q e@(Event e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(Const _) = concatM [q e]
	where m = queryM q
queryM q e@(SigAt e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
--queryM q e@(Case ce cs) = q e, q ce, concatMap (m . snd) cs
--	where m = queryM q
queryM q e@(Var _) = concatM [q e]
queryM q e@(Nil) = concatM [q e]

queryM q e@(Box e1) = concatM [q e, m e1]
	where m = queryM q
queryM q e@(Translate e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(Colour e1 e2) = concatM [q e, m e1, m e2]
	where m = queryM q
queryM q e@(HasType _ e1) = concatM [q e, m e1]
	where m = queryM q

queryM q e = fail $ "queryM: unknown expr "++show  e 


mapEM :: (E-> TravM E)-> E -> TravM E
mapEM f e = mapEM' e
    where m = withPath e . mapEM f 
          mapEM' (If p c a) = ( return If `ap` m p `ap` m c `ap` m a) >>= f
          mapEM' (Lam n bd) = (withBvars [n] $ return (Lam n) `ap` m bd) >>= f
          mapEM' (App le ae) = (return App `ap` m le `ap` m ae) >>= f
          mapEM' (Var n) = f $ Var n
          mapEM' (Sig s) = (return Sig `ap` m s) >>= f
          mapEM' (SigVal s) = (return SigVal `ap` m s) >>= f
          mapEM' (SigDelay s1 s2) = (return SigDelay `ap` m s1 `ap` m s2) >>= f
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
          mapEM' (Const c) = f $ Const c
          mapEM' (Nil) = (f Nil)

          mapEM' (Box s1) = (return Box `ap` m s1) >>= f
          mapEM' (Translate s1 s2) = (return Translate `ap` m s1 `ap` m s2) >>= f
          mapEM' (Colour s1 s2) = (return Colour `ap` m s1 `ap` m s2) >>= f
          mapEM' (HasType t s2) = (return (HasType t) `ap` m s2) >>= f

          mapEM' (LetE ses er) = (
              withBvars (map fst ses) $ 
              return LetE `ap` mapM (\(n,e)-> do e' <- m e
                                                 return (n, e')) ses 
                          `ap` m er) >>= f
          mapEM' (Switch ses er) = (
              return Switch `ap` mapM (\(e1,e2)-> do e1' <- m e1
                                                     e2' <- m e2
                                                     return (e1',e2')) ses 
                            `ap` m er) >>= f
          -- mapE f e = error $ "mapE: unknown expr "++show e 
              

ifM :: Monad m => m Bool -> m a ->  m a -> m a
ifM mp mc ma = do p <- mp
                  if p
                     then mc
                     else ma
