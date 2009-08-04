module TypeCheck where

import Types
import Expr
import Traverse
import EvalM
import Control.Monad.State.Strict
import PrettyPrint
import TNUtils
import Data.Maybe
import Data.List
import BuiltIn

allDeclaredTypes :: TravM [(String, T)]
allDeclaredTypes = do
  ds <- decls `fmap` get
  return [(nm, t) | DeclareType nm t <- ds]

typeCheck :: TravM ()
typeCheck = do addBuiltinsTypeAnnos
               labelUnspecifiedTypes 
               deriveTypeConstraints 
               traceM ""
               traceM "derived constraints"
               traceTyConstraints
               solveConstraints
               traceM ""
               traceM "unified constraints"
               traceTyConstraints
               applySolution
               traceDecls
               

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

addBuiltinsTypeAnnos :: TravM ()
addBuiltinsTypeAnnos = mapM_ addTyAnno bivs
    where addTyAnno (BiV nm ty _) = insertAtTop [DeclareType nm ty]

labelUnspecifiedTypes :: TravM ()
labelUnspecifiedTypes = mapDE (mapEM lUT) >> topLevelMap
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

deriveTypeConstraints :: TravM ()
deriveTypeConstraints  = do decTys <- allDeclaredTypes
                            forM_ decTys $ \(nm,t)-> do
                              mdefn <- safeLookUp nm
                              case mdefn of
                                Just defn -> checkTy defn t >> return ()
                                Nothing -> return ()

solveConstraints :: TravM ()
solveConstraints = do constrs <- tyConstraints `fmap` get
                      let constrs' = nub . unify . addRepetitions $ constrs 
                      traceM ""
                      traceM "added repetitins"
                      traceConstraints . nub . addRepetitions $ constrs 
                      setter $ \s-> s {tyConstraints = constrs'}

addRepetitions :: [(T,T)] -> [(T,T)]
addRepetitions constrs = constrs ++ concatMap aR' constrs
    where aR' p = aR p ++ (aR . swap $ p) 
          aR (t1@(UnknownT unm),t2) = 
              let others = [ ot | (thisTy, ot) <- constrs, ot /= t2, thisTy == t1 ]++
                           [ ot | (ot, thisTy) <- constrs, ot /= t2, thisTy == t1 ]
              in combinations $ t2:others

          aR _ = []
--combinations . map snd . filter ((/=t2) . snd) . filter ((==t1) . fst) $ constrs
 
combinations :: [a] -> [(a,a)]
combinations [] = []
combinations (x:xs) = map ((,) x) xs ++ combinations xs

applySolution :: TravM ()
applySolution = do cns <- tyConstraints `fmap` get
                   mapD (aS cns)
    where aS cns (DeclareType nm ut@(UnknownT _)) = do
            let newTy =  makePoly $ substT cns ut
            traceM ""
            traceM $ "foo: "++^ut++" --> "++^newTy
            traceM "new constraints :"
            --lookup ut in cns
            let oldMatches = zip (lookupMany ut cns) $ repeat newTy
            let newConstrs = ((ut,newTy):oldMatches++cns)
            --unity also ut with other
            --traceConstraints $  unify ((ut,newTy):cns)
            alterDefinition nm . substTyInE $ unify newConstrs
            return $ DeclareType nm newTy
          aS _ d = return d

lookupMany :: Eq a => a -> [(a,b)] -> [b]
lookupMany x = map snd . filter ((==x) . fst)

substTyInE subs = mapE sTy
    where sTy e@(Lam nm t bd) = Lam nm (substT subs t) bd                              
          sTy e = e

makePoly :: T -> T
makePoly t = let unknowns = [ UnknownT nm | UnknownT nm <- flatT t]
                 subs = zip unknowns $ map (TyVar . unitList) ['a'..'z']
             in substT subs t

unitList x = [x]

checkTy :: E -> T -> TravM T
checkTy (If p c a) t = do checkTy p BoolT
                          checkTy c t
                          checkTy a t
                          return t
checkTy (Var nm) t = do vty <- symbolType nm
                        vtyRefresh <- refresh vty
                        addTyConstraint (vtyRefresh, t)
                        return t
checkTy (Const v) t@(UnknownT nm) = do addTyConstraint (typeOfVal v, t)
                                       return $ typeOfVal v
checkTy (Const v) t | typeOfVal v `isSubtypeOf` t = return t
                    | otherwise = tyFail [ppVal v," is not of type ",ppType t]
checkTy (App e1 e2) tfinal = do targ <- UnknownT `fmap` (genSym "checkApp")
                                lt1 <- checkTy e1 (LamT targ tfinal)
                                case lt1 of 
                                  LamT targ1 tfinal1 -> do checkTy e2 targ1
                                                           return tfinal1
                                  t -> tyFail [ppType t," does not match ",
                                               ppType targ,"->", ppType tfinal] 
checkTy e@(Lam nm targ bd) tlam = do tfinal <- UnknownT `fmap` (genSym "checkLam")
                                     addTyConstraint (tlam, LamT targ tfinal)
                                     tfinal1 <- withPath e $ checkTy bd tfinal
                                     return $ LamT targ tfinal1
checkTy (Pair e1 e2) t@(PairT t1 t2) = do checkTy e1 t1
                                          checkTy e2 t2
                                          return t
checkTy (M2 op e1 e2) t = do addTyConstraint (t, NumT Nothing)
                             t1 <- checkTy e1 t 
                             t2 <- checkTy e2 t 
                             addTyConstraint (t1, t2)
                             addTyConstraint (t1, t)
                             return t
                                                            
checkTy e t = return t

tyFail s = fail $ "Type check fails: "++concat s

threeToPairR (a,b,c) = (a, (b,c))

unify :: [(T,T)] -> [(T,T)]
unify [] = []
unify (c@(UnknownT unm, UnknownT unm1):constrs) | unm == unm1 = unify constrs
                                                | otherwise = c:unify constrs
unify (c@(UnknownT unm, t2):constrs) = c:unify constrs
unify (c@(t1, UnknownT unm):constrs) = swap c:unify constrs
unify ((LamT t1 t2, LamT t3 t4):constrs) = unify $ constrs++[(t1, t3), (t2,t4)]
unify ((PairT t1 t2, PairT t3 t4):constrs) = unify $ constrs++[(t1, t3), (t2,t4)]
unify ((ListT t1, ListT t2):constrs) = unify $ constrs++[(t1, t2)]
unify ((t1,t2):constrs) | t1 == t2 || 
                          t1 `isSubtypeOf` t2 || 
                          t2 `isSubtypeOf` t1 = unify constrs
                        | t1 `partOfTy` t2 || 
                          t1 `partOfTy` t2 = error $ "infinite type: "++^t1 ++"="++^t2
                        | otherwise = error $ "cannot unify: "++^t1 ++"="++^t2

s ++^ ty = s++(ppType ty) 
swap (a,b) = (b,a)

substT :: [(T,T)] -> T -> T
substT subs t = if t `elem` (map fst subs) 
                   then substT subs . fromJust $ lookup t subs
                   else recSubstT subs t

recSubstT :: [(T,T)] -> T -> T
recSubstT subs (PairT t1 t2) = PairT (substT subs t1) (substT subs t2)
recSubstT subs (LamT t1 t2) = LamT (substT subs t1) (substT subs t2)
recSubstT subs (ListT t1) = ListT (substT subs t1)
recSubstT subs t = t 

refresh :: T -> TravM T
refresh t = do 
  let tVars = tyVarsIn t
  freshes <- mapM genSym tVars
  return $ substT (zip (map TyVar tVars) (map UnknownT freshes)) t


tyVarsIn t = catMaybes . map tVI $ flatT t
             where tVI (TyVar s) = Just s
                   tVI _ = Nothing

partOfTy t1 t2 = t1 `elem` flatT t2

flatT t@(PairT t1 t2) = t:flatT t1++flatT t2
flatT t@(LamT t1 t2) = t:flatT t1++flatT t2
flatT t@(ListT tl) = t:flatT tl
flatT t = [t]






{-tyVarsIn (TyVar v) = [v]
tyVarsIn (PairT t1 t2) = tyVarsIn t1++ tyVarsIn t2
tyVarsIn (LamT t1 t2) = tyVarsIn t1++ tyVarsIn t2
tyVarsIn (ListT t1) = tyVarsIn t1
tyVarsIn t = [] -}

--partOfTy t1 t2 | t1 == t2 = True
--               | otherwise = partOfTyAux t1 t2
--partOfTyAux t1 (PairT t2 t3) =  partOfTy t1 t2 || partOfTy t1 t3
--partOfTyAux t1 (LamT t2 t3) =  partOfTy t1 t2 || partOfTy t1 t3

--queryT :: (T-> [a]) -> T -> [a]
--queryT q (Pair = 
