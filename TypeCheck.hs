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
import Data.Ord
import Debug.Trace

allDeclaredTypes :: TravM [(String, T)]
allDeclaredTypes = do
  ds <- decls `fmap` get
  return [(nm, t) | DeclareType nm t <- ds]

allDefinitions :: TravM [Declare]
allDefinitions = do
  ds <- decls `fmap` get
  return [l | l@(Let nm e) <- ds]


typeCheck :: TravM ()
typeCheck = do addBuiltinsTypeAnnos
               defs <- allDefinitions
               mapM_ tyCheckD defs
               --labelUnspecifiedTypes 
               --deriveTypeConstraints 
               --traceDecls
               --traceM ""
               --traceM "derived constraints"
               --traceTyConstraints
               --solveConstraints
               --traceM ""
               --traceM "unified constraints"
               --traceTyConstraints
               --applySolution
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

tyCheckD d@(Let nm e)=do decTys <- allDeclaredTypes
                         clearTyConstraints
                         case lookup nm decTys of
                           Just t -> do tinf <- checkTy e
                                        addTyConstraint (t,tinf)
                                        solveConstraints
                                        {-if t == tinf
                                           then return ()
                                           else tyFail ["expected: ", ppType t, ", inferred: ", ppType tinf] -}
                           Nothing -> do e' <- mapEM lUT e
                                         t <- UnknownT `fmap` genSym nm
                                         insertBefore [DeclareType nm t]
                                         --alterDefinition nm $ const e'
                                         --when (nm=="vm") $ do traceTyConstraints
                         --traceM "definition of fst:"
                                         --                      traceDefn "fst"
                                         tcalc <- checkTy e'
                                         addTyConstraint (t,tcalc)
                                         --traceM ""
                                         --traceM2 "solving for " nm
                                         --traceTyConstraints
                                         solveConstraints
                                         applySolution
                                         markChange
                                         
                                         --alterTypeDefinition nm 
                                         return ()
--tyCheckD d = return d
                         

addBuiltinsTypeAnnos :: TravM ()
addBuiltinsTypeAnnos = mapM_ addTyAnno bivs
    where addTyAnno (BiV nm ty _) = insertAtTop [DeclareType nm ty]

labelUnspecifiedTypes :: TravM ()
labelUnspecifiedTypes = mapDE (mapEM lUT) >> topLevelMap
    where 
          topLevelMap = do decTys <- allDeclaredTypes
                           mapD (lUT' decTys)
          lUT' tenv d@(Let nm bd) | nm `elem` (map fst tenv) = return d
                                  | otherwise = do tvar <- genSym nm
                                                   insertBefore [DeclareType nm $  UnknownT tvar]
                                                   return d
          lUT' _ d = return d

lUT (Lam nm UnspecifiedT bd) = do tvar <- genSym nm
                                  return (Lam nm (UnknownT tvar) bd)
lUT (LetE assocs bd) = return LetE `ap` mapM lUTLet assocs `ap` return bd
lUT (Case et pates) = return Case `ap` return et `ap` forM pates (\(pat,e)-> return (,) `ap` 
                                                                             lUTPat pat `ap` 
                                                                             return e)
lUT e = return e
lUTLet (nm,UnspecifiedT,e) = do tvar <- genSym nm
                                return (nm,UnknownT tvar,e)
lUTLet l = return l
lUTPat (PatVar nm UnspecifiedT) = do tvar <- genSym nm
                                     return (PatVar nm $ UnknownT tvar)
lUTPat (PatPair p1 p2) = do p1' <- lUTPat p1
                            p2' <- lUTPat p2
                            return (PatPair p1' p2')
lUTPat (PatCons p1 p2) = do p1' <- lUTPat p1
                            p2' <- lUTPat p2
                            return (PatCons p1' p2')
lUTPat p = return p

symbolType :: String -> TravM (T, Bool)
symbolType nm = do path <- exprPath `fmap` get
                   --mayDefs <- ca mapM (definesTy nm) path
                   case catMaybes $ map (definesTy nm) path of
                     [] ->  lookupGlobal nm
                     (t:_) -> return (t, False)
    where definesTy :: String -> E -> Maybe T
          definesTy nm (Lam nm1 t _) | nm== nm1 = Just t
                                     | otherwise = Nothing
          definesTy nm (LetE assocs _) = (fst) `fmap` (lookup nm $ map threeToPairR assocs)
          definesTy nm (Case _ ((pat,e):[])) = safeHead [ t | PatVar nmv t <- flatPat pat, nm == nmv]
          definesTy _ _ = Nothing
          lookupGlobal :: String -> TravM (T,Bool)
          lookupGlobal nm = do decTys <- allDeclaredTypes
                               case lookup nm decTys of
                                 Just t -> return (t,True)
                                 Nothing -> error $ "cannot find symbol: "++nm

deriveTypeConstraints :: TravM ()
deriveTypeConstraints  = do decTys <- allDeclaredTypes
                            forM_ decTys $ \(nm,t)-> do
                              mdefn <- safeLookUp nm
                              case mdefn of
                                Just defn -> do tcalc <- checkTy defn
                                                addTyConstraint (t,tcalc)
                                                return ()
                                Nothing -> return ()

solveConstraints :: TravM ()
solveConstraints = do constrs <- tyConstraints `fmap` get
                      let constrs' = nub . map (\(n,t)-> (UnknownT n, t)) . solve {-. addRepetitions $ -} $ constrs 
                      --traceM ""
                      --traceM "solved repetitins"
                      --traceConstraints {-. addRepetitions $ -} constrs'
                      setter $ \s-> s {tyConstraints = constrs'}


hasUnknownT t = not . null $ [() | UnknownT _ <- flatT t]


applySolution :: TravM ()
applySolution = do cns <- tyConstraints `fmap` get
                   mapD (aS cns)
    where aS cns (DeclareType nm ut@(UnknownT _)) = do
            let newTy =  makePoly $ substT cns ut
            let newCns = ((ut,newTy):cns)
            --traceM ""
            --traceM $ "foo: "++^ut++" --> "++^newTy
            --traceM "new constraints :"
            --lookup ut in cns
            --let oldMatches = zip (lookupMany ut cns) $ repeat newTy
            --let newConstrs = unify ((ut,newTy):oldMatches++cns)
            --unity also ut with other
            --traceConstraints $  newConstrs
            alterDefinition nm . substTyInE $ newCns -- newConstrs
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

--          constr ctx (Const v) = 

checkTy :: E -> TravM T
checkTy (If p c a) = do tp <- checkTy p 
                        tc <- checkTy c
                        ta <- checkTy a
                        addTyConstraint (tp, BoolT)
                        addTyConstraint (tc, ta)                       
                        return tc
checkTy (Var nm) = do (vty, isGlobal) <- symbolType nm 
                      if isGlobal
                         then do reTy <- refresh vty
                                 --traceM2 "original" vty 
                                 --traceM2 "refreshed" reTy 
                                 return reTy
                         else return vty                     
checkTy (Const v) = return . generaliseInts $ typeOfVal v
    where generaliseInts (NumT (Just IntT)) = NumT Nothing
          generaliseInts t = t 
checkTy (App e1 e2) = do t1 <- checkTy e1
                         t2 <- checkTy e2
                         ty <- UnknownT `fmap` (genSym "checkApp")
                         addTyConstraint (t1, LamT t2 ty)
                         return ty

checkTy e@(Lam nm targ bd) = do tres <- withPath e $ checkTy bd
                                --addTyConstraint (
                                return $ LamT targ tres
checkTy (Pair e1 e2) = do t1 <- checkTy e1
                          t2 <- checkTy e2
                          return $ PairT t1 t2
checkTy (M2 op e1 e2) = do t1 <- checkTy e1 
                           t2 <- checkTy e2 
                           addTyConstraint (t1, NumT Nothing)
                           addTyConstraint (t2, NumT Nothing)
                           return t1
checkTy (M1 op e1) = do t1 <- checkTy e1 
                        addTyConstraint (t1, NumT Nothing)
                        return t1
checkTy (Cmp op e1 e2) = do t1 <- checkTy e1 
                            t2 <- checkTy e2 
                            addTyConstraint (t1, NumT Nothing)
                            addTyConstraint (t2, NumT Nothing)
                            return BoolT
checkTy (Nil) = do telem <- UnknownT `fmap` (genSym "checkNil")
                   return $ ListT telem
checkTy (Cons x xs) = do ty1 <- checkTy x 
                         ty2 <- checkTy xs
                         let ty = ListT ty1
                         addTyConstraint (ty2,ty)
                         return ty
checkTy (Case et pates) = do t1 <- checkTy et
                             ts <- forM pates $ \(pat,e) -> do
                                     patTy <- tyPat pat
                                     addTyConstraint (t1,patTy)
                                     withPath (Case et [(pat,e)]) $ checkTy e
                             addManyConstraints ts
                             return $ head ts
checkTy e@(LetE ses es) = do te <- withPath e $ checkTy es
                             forM_ ses $ \(nm, t,el)-> withPath e $ do tc <- checkTy el
                                                                       addTyConstraint (t,tc)
                             return te
checkTy e@(And e1 e2) = do t1 <- checkTy e1 
                           t2 <- checkTy e2 
                           addTyConstraint (t1, BoolT)
                           addTyConstraint (t2, BoolT)
                           return BoolT
checkTy e@(Or e1 e2)  = do t1 <- checkTy e1 
                           t2 <- checkTy e2 
                           addTyConstraint (t1, BoolT)
                           addTyConstraint (t2, BoolT)
                           return BoolT
checkTy e@(Not e1) = do t1 <- checkTy e1 
                        addTyConstraint (t1, BoolT)
                        return BoolT
checkTy (Sig e) = do te <- checkTy e
                     return $ SignalT te
checkTy (SigVal s) = do ts <- checkTy s
                        case ts of
                          SignalT et -> return et
                          _ -> do telem <- UnknownT `fmap` (genSym "checkSigVal")
                                  addTyConstraint (ts, SignalT telem)
                                  return $ telem
checkTy (SigAt tm s) = do ty1 <- checkTy tm 
                          tys <- checkTy s
                          addTyConstraint (ty1, NumT (Just RealT))
                          case tys of
                            SignalT tval -> return tval
                            _ -> do telem <- UnknownT `fmap` (genSym "checkSigAt")
                                    addTyConstraint (tys, SignalT telem)
                                    return $ telem

checkTy (SigDelay s v0) = do tyv0 <- checkTy v0 
                             tys <- checkTy s
                             case tys of
                               SignalT tval -> do addTyConstraint (tyv0, tval)
                                                  return tys
                               _ -> do telem <- UnknownT `fmap` (genSym "checkSigDelay")
                                       addTyConstraint (tys, SignalT telem)
                                       addTyConstraint (tyv0, telem)
                                       return $ SignalT telem
                          
checkTy (Event e) = do tev <- checkTy e
                       --traceM2 "event expr type: " tev
                       telem <- UnknownT `fmap` (genSym "checkEvent")
                       addTyConstraint (tev, ListT $ PairT (NumT (Just RealT)) telem)
                       return tev -- . ListT $ PairT (NumT (Just RealT)) telem

checkTy (Box e) = do vecTy <-checkTy e
                     addTyConstraint (vecTy, vec3Ty)
                     return ShapeT
checkTy (Colour ce shpe) = do shpt <- checkTy shpe
                              colt <- checkTy ce
                              addTyConstraint (shpt, ShapeT)
                              addTyConstraint (colt, vec3Ty)
                              return ShapeT
checkTy (Translate ve shpe) = do shpt <- checkTy shpe
                                 vect <- checkTy ve
                                 addTyConstraint (shpt, ShapeT)
                                 addTyConstraint (vect, vec3Ty)
                                 return ShapeT
checkTy (Switch eslams es)  = do sigTy <- checkTy es
                                 telem <- case sigTy of
                                            SignalT telem' -> return telem'
                                            _ -> do telem' <- UnknownT `fmap` (genSym "checkSigAt")
                                                    addTyConstraint (sigTy, SignalT telem')
                                                    return $ telem'

                                 evTys <- forM eslams $ \(ev, slam) -> do
                                            evsTy <- checkTy ev
                                            slamTy <- checkTy slam
                                            tyA <- UnknownT `fmap` (genSym "checkSwitch")
                                            addTyConstraint (evsTy, ListT $ PairT (NumT (Just RealT)) tyA)
                                            addTyConstraint (slamTy, LamT realT (LamT tyA (SignalT telem)))
                                            return tyA
                                 addManyConstraints evTys
                                 return $ SignalT telem

checkTy e = error $ "checkTy: unknown expr: "++pp e

vec3Ty = PairT (PairT realT realT) realT

addManyConstraints :: [T] -> TravM ()
addManyConstraints [] = return ()
addManyConstraints (t:ts) = forM_ ts $ \t1 -> addTyConstraint (t,t1)

tyPat :: Pat -> TravM T
tyPat (PatVar nm t) = return t
tyPat (PatLit v) = return $ typeOfVal v
tyPat (PatIgnore) = UnknownT `fmap` (genSym "patIgnore")
tyPat (PatNil) = (ListT . UnknownT) `fmap` (genSym "patNil")
tyPat (PatPair p1 p2) = do t1 <- tyPat p1
                           t2 <- tyPat p2
                           return $ PairT t1 t2
tyPat (PatCons p1 p2) = do t1 <- tyPat p1
                           t2 <- tyPat p2
                           let ty = ListT t1
                           addTyConstraint (t2,ty)
                           return ty                   

--checkTy e = return t

tyFail s = fail $ "Type check fails: "++concat s

threeToPairR (a,b,c) = (a, (b,c))

--plzoo poly
solve :: [(T,T)] -> [(String,T)]
solve eqs = solv eqs []
    where solv :: [(T,T)] -> [(String,T)] -> [(String,T)]
          solv [] sbst = sbst
          solv ((PairT t1 t2, PairT t3 t4):eq) sbst = solv ((t1, t3):(t2,t4):eq) sbst
          solv ((LamT t1 t2, LamT t3 t4):eq) sbst = solv ((t1, t3):(t2,t4):eq) sbst
          solv ((ListT t1, ListT t3):eq) sbst = solv ((t1, t3):eq) sbst
          solv ((SignalT t1, SignalT t3):eq) sbst = solv ((t1, t3):eq) sbst
          solv ((t1@(UnknownT nm), t2):eq) sbst | t1 == t2 = solv eq sbst
                                                | not (t1 `partOfTy` t2) =
                                                    let ts = substT [(t1, t2)] in
                                                    solv (map (\(ty1, ty2) -> (ts ty1, ts ty2)) eq)
                                                         ((nm, t2):(map (\(s,t) -> (s, ts t)) sbst))
                                                | otherwise = error $ "cannot unify: "++^t1 ++"="++^t2
          solv ((t2, t1@(UnknownT nm)):eq) sbst | not (t1 `partOfTy` t2) =
                                                    let ts = substT [(t1, t2)] in
                                                    solv (map (\(ty1, ty2) -> (ts ty1, ts ty2)) eq)
                                                         ((nm, t2):(map (\(s,t) -> (s, ts t)) sbst))
                                                | otherwise = error $ "cannot unify: "++^t2 ++"="++^t1
          solv ((t1,t2):eq) sbst | t1 == t2 || 
                                   t1 `isSubtypeOf` t2 || 
                                   t2 `isSubtypeOf` t1 =  solv eq sbst
                                 | otherwise = error $ "cannot unify: "++^t1 ++"="++^t2


s ++^ ty = s++(ppType ty) 
swap (a,b) = (b,a)

substT :: [(T,T)] -> T -> T
substT subs t = 
   {- trace (ppType t) $-} if t `elem` (map fst subs) 
                              then substT subs . {- mostSpecific -} head $ lookupMany t subs
                              else recSubstT subs t

recSubstT :: [(T,T)] -> T -> T
recSubstT subs (PairT t1 t2) = PairT (substT subs t1) (substT subs t2)
recSubstT subs (LamT t1 t2) = LamT (substT subs t1) (substT subs t2)
recSubstT subs (ListT t1) = ListT (substT subs t1)
recSubstT subs (SignalT t1) = SignalT (substT subs t1)
recSubstT subs t = t 

refresh :: T -> TravM T
refresh t = do 
  let tVars = tyVarsIn t
  freshes <- mapM (genSym . tyVarName) tVars
  return $ substT (zip tVars (map UnknownT freshes)) t


tyVarsIn t = catMaybes . map tVI $ flatT t
             where tVI t@(TyVar s) = Just t
                   tVI t@(UnknownT s) = Just t
                   tVI _ = Nothing

tyVarName (TyVar s) = s
tyVarName (UnknownT s) = s
tyVarName t = ppType t

partOfTy t1 t2 = t1 `elem` flatT t2

flatT t@(PairT t1 t2) = t:flatT t1++flatT t2
flatT t@(LamT t1 t2) = t:flatT t1++flatT t2
flatT t@(ListT tl) = t:flatT tl
flatT t = [t]

flatPat p@(PatPair p1 p2) = p:flatPat p1++flatPat p2
flatPat p@(PatCons p1 p2) = p:flatPat p1++flatPat p2
flatPat p = [p]

constraintsOf gctx e = constr [] e
    where constr ctx (Var nm) =
              case lookup nm ctx of
                Just t -> return t
                Nothing -> case lookup nm gctx of
                             Just t -> do ty <- refresh t
                                          return ty
                             Nothing -> error $ "cannot find "++nm

mostSpecific :: [T] -> T
mostSpecific = maximumBy (comparing rank)
    where rank (TyVar _) = 5
          rank (UnknownT _) = 4
          rank (NumT (Just _)) = 7
          rank (NumT Nothing) = 6
          rank _ = 7

maximumOn :: Ord b => (a->b) -> [a] -> a
maximumOn f xs = fst . maximumBy (comparing snd) $ zip xs (map f xs)
               

combinations :: [a] -> [(a,a)]
combinations [] = []
combinations (x:xs) = map ((,) x) xs ++ combinations xs

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
