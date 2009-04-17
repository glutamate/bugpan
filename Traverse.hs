module Traverse where

import Expr
import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.List as L

data TravS = TravS { counter :: Int,
                     decls :: [Declare],
                     env ::  [(String, E)],
                     boundVars :: [String]
                   }

type TravM = StateT TravS Identity

getEnv = env `fmap` get

getDecls = decls `fmap` get

runTravM :: [Declare] -> [(String, E)] -> TravM a -> (a, [Declare])
runTravM decs env tx 
    = let initS = TravS 0 decs env []
          (x, TravS _ decsFinal _ _) = runIdentity $ runStateT tx initS
      in (x, decsFinal)

gensym :: String -> TravM String
gensym base = do (TravS ctr d env bvs) <- get
                 put (TravS (ctr+1) d env bvs)
                 return $ base ++ "_" ++ (show ctr)

withEnv :: String -> E -> TravM a -> TravM a
withEnv n e tx = do (TravS ctr d env bvs) <- get
                    put (TravS ctr d ((n,e):env) bvs)
                    x <- tx
                    (TravS ctr' d' _ bvs) <- get
                    put (TravS ctr' d' env bvs)
                    return x

withBvar n tx  = do (TravS ctr d env bvs) <- get
                    put (TravS ctr d env (n:bvs))
                    x <- tx
                    (TravS ctr' d' env' _) <- get
                    put (TravS ctr' d' env' bvs)
                    return x

lookUp :: String -> TravM (E)
lookUp nm = do env <- getEnv
               case L.lookup nm env of
                 Just e -> return e
                 Nothing -> lookUpInDecls
    where lookUpInDecls = do ds <- declsToEnv `fmap` getDecls
                             case L.lookup nm ds of
                               Just e -> return e
                               Nothing -> fail $ "lookUp: can't find "++nm
                             
          
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
{-
queryM q e@(LetE ses er) = q e ++ m er ++ concatMap (m . snd) ses  
	where m = queryM q-}

queryM q e@(Lam n bd) = withBvar n $ concatM [q e, m bd]
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
queryM q e = fail $ "queryM: unknown expr "++show  e 

mapEM :: (E-> TravM E)-> E -> TravM E
mapEM f e = mapEM' e
    where m = mapEM f 
          mapEM' (If p c a) =  return If `ap` m p `ap` m c `ap` m a >>= f
              

