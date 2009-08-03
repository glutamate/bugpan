module TypeCheck where

import Types
import Expr
import Traverse
import EvalM
import Control.Monad.State.Strict
import PrettyPrint
import TNUtils
import Data.Maybe
 

allDeclaredTypes :: TravM [(String, T)]
allDeclaredTypes = do
  ds <- decls `fmap` get
  return [(nm, t) | DeclareType nm t <- ds]

typeCheck :: TravM ()
typeCheck = labelUnspecifiedTypes >> deriveStageConstraints >> traceTyConstraints

{-mapD tc
    where tc d@(DeclareType nm declt) = do
            defn <- lookUp nm
            tenv <- allDeclaredTypes
            --traceM $ nm++": "++(ppType declt)
            case exprType tenv defn (Just declt) of
              Nothing -> error $ "Unable to infer type "++ppType declt++" for expression "++nm
              Just t1 -> case unifyTypes t1 declt of
                           Just t -> return d
                           Nothing -> let err = concat ["Mismatched types for ", nm,
                                                         "\nDeclared type: ", show declt,
                                                         "\nInferred type: ", show declt,"\n"]
                                      in error err
          tc d = return d -}

labelUnspecifiedTypes :: TravM ()
labelUnspecifiedTypes = mapDE (mapEM lUT) >> topLevelMap >> traceDecls
    where lUT (Lam nm UnspecifiedT bd) = do tvar <- genSym nm
                                            return (Lam nm (UnknownT tvar) bd)
          lUT (LetE assocs bd) = return LetE `ap` mapM lUTLet assocs `ap` return bd
          lUT e = return e
          lUTLet (nm,UnspecifiedT,e) = do tvar <- genSym nm
                                          return (nm,UnknownT tvar,e)
          lUTLet l = return l
          lUTPat (PatVar nm UnspecifiedT) = do tvar <- genSym nm
                                               return (PatVar nm $ UnknownT tvar)
          lUTPat p = return p
          topLevelMap = do decTys <- allDeclaredTypes
                           mapD (lUT' decTys)
          lUT' tenv d@(Let nm bd) | nm `elem` (map fst tenv) = return d
                                  | otherwise = do tvar <- genSym nm
                                                   insertBefore [DeclareType nm $  UnknownT tvar]
                                                   return d
          lUT' _ d = return d

symbolType :: String -> TravM T
symbolType nm = do path <- exprPath `fmap` get
                   --mayDefs <- ca mapM (definesTy nm) path
                   case catMaybes $ map (definesTy nm) path of
                     [] ->  lookupGlobal nm
                     (t:_) -> return t
    where definesTy :: String -> E -> Maybe T
          definesTy nm (Lam nm1 t _) | nm== nm1 = Just t
                                     | otherwise = Nothing
          definesTy nm (LetE assocs _) = (fst) `fmap` (lookup nm $ map threeToPairR assocs)
          definesTy _ _ = Nothing
          lookupGlobal :: String -> TravM T
          lookupGlobal nm = do decTys <- allDeclaredTypes
                               return . fromJust $ lookup nm decTys

deriveStageConstraints :: TravM ()
deriveStageConstraints = do decTys <- allDeclaredTypes
                            forM_ decTys $ \(nm,t)-> do
                              defn <- lookUp nm
                              checkTy defn t

checkTy :: E -> T -> TravM T
checkTy (If p c a) t = do checkTy p BoolT
                          checkTy c t
                          checkTy a t
                          return t
checkTy (Var nm) t = do vty <- symbolType nm
                        addTyConstraint (vty, t)
                        return t
checkTy (Const v) t@(UnknownT nm) = do addTyConstraint (typeOfVal v, t)
                                       return $ typeOfVal v
checkTy (Const v) t | typeOfVal v `isSubtypeOf` t = return t
                    | otherwise = fail $ "Type check fails: "++ppVal v++" is not of type "++ppType t
checkTy (App e1 e2) tfinal = do targ <- UnknownT `fmap` (genSym "checkApp")
                                LamT targ1 tfinal1 <- checkTy e1 (LamT targ tfinal)
                                checkTy e2 targ1
                                return tfinal1
                                
                                
checkTy e t = return t

threeToPairR (a,b,c) = (a, (b,c))