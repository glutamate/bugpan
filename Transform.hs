module Transform where

import Expr
import Eval
--import Run 
import EvalM
import Control.Monad
import Numbers
import Data.Char 
import Data.List (partition, intercalate)
import Traverse
import Control.Monad.State.Strict
import Debug.Trace
import BuiltIn
import TNUtils
import TypeCheck
import Data.List
import PrettyPrint
import CompiledSrcsSinks

substHasSigs :: TravM ()
substHasSigs = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            --trace ("substHasSigs:"++show e) return ()
            ifM (dontLookUp nm)
                (return e)
                $ do defn <- lookUp nm
                     ifM (liftM2 (&&) (hasSig $ stripSig defn) (not `fmap` recursiveSigGen nm defn))
                         ({- do ph <- exprPath `fmap` get
                             traceM $ "substHasSig "++nm
                             traceM $ "path "++(unlines $ map pp pth) -}
                          {-if nm == "convolve"
                             then return e
                             else -} return $ App (defn) arg)
                         (return e) 
          subst e = return e

atsArgument :: E -> Bool
atsArgument e = let args = getEArgs e
                    getEArgs (Lam nm _ bd) = nm:getEArgs bd
                    getEArgs _ = []
                    getAts (SigAt _ (Var nm1)) = [nm1 `elem` args]
                    getAts _ = []
                in nonempty . filter id $ queryE getAts e


recursiveSigGenApplication = mapDE tle
    where tle e =  case fst $ unApp e of 
                     Var nm -> rSGA e nm
                     _ -> return e
          unSig (Sig e) = e
          unSig e = e
          rSGA e nm = do
            ifM (dontLookUp nm)
                (return e)
                $ do defn <- lookUp nm
                     recSigGen <- recursiveSigGen nm defn
                     --traceM "foo"
                     if not recSigGen
                        then return e
                        else do f <- genSym $ nm++"_recSig"
                                let (args, innerDefn) = unLam defn
                                let subInner = subVar nm (Var f) innerDefn
                                let sigDefn = lamMany (unSig subInner) args
                                let newInnerDefn = Sig (LetE [(PatVar f UnspecifiedT, sigDefn)] 
                                                             (appMany (Var f) . snd $ unApp e))
                                --traceM "recursiveSigGenApplication"
                                --traceM nm
                                --traceM $ pp defn
                                --traceM $ pp e
                                --t-raceM $ pp newInnerDefn
                                return newInnerDefn

appMany el [] = el
appMany el (e:es) = appMany (App el e) es

lamMany bd [] = bd
lamMany bd (nm:nms) = lamMany (Lam nm UnspecifiedT bd) nms

unInnerSig (Lam nm t bd) = Lam nm t $ unInnerSig bd
unInnerSig (Sig e) = e
unInnerSig e = e

unApp e = unApp' e []
unApp' (App lam arg) args = unApp' lam (arg:args)
unApp' e nms = (e,nms)

recursiveSigGen :: String -> E -> TravM Bool
recursiveSigGen nm e = do let (_, unlame) = unLam e
                          let isSig e = (==IsSig) `fmap` isSignalOrEvt e 
                          issig <- isSig unlame
                          let isrec = Var nm `elem` flatE unlame
                          return $ isrec && issig



substGetsSigs :: TravM ()
substGetsSigs = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            ifM (dontLookUp nm)
                (return e)
                $ do defn <- lookUp nm
                     ifM (hasSig arg)
                         (return $ App (defn) arg)
                         (return e) 
          subst e = return e

substMakesShape :: TravM ()
substMakesShape = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            ifM (dontLookUp nm)
                (return e)
                $ do defn <- lookUp nm
                     ifM (makesShape defn)
                         (return $ App (defn) arg)
                         (return e) 
          subst e = return e


betaRedHasSigs :: TravM ()
betaRedHasSigs = mapDE $ \tle -> mapEM brhs tle
    where brhs e = ifM (hasSig e)
                       (return . betaContract $ e)
                       (return e)

betaRedMakesShape :: TravM ()
betaRedMakesShape = mapDE $ \tle -> mapEM brms tle
    where brms e = ifM (makesShape e)
                       (return . betaContract $ e)
                       (return e)


betaContract :: E -> E
betaContract eo@(App (Lam nm t bd) arg) = mapE bar bd
    where bar e@(Var n') | n' == nm =  arg -- doesnt work if shadows
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
unDelays = mapDE unDelays' 
    where unDelays' e@(SigDelay (Var _) _) = return e
          unDelays' tle = mapEM undel tle
          undel e@(SigDelay (Var nm) initE) = do
            --assumes inite has no bound vars. FIXME
            ifM (inBoundVars nm)
                (return e)
                (do dnm <- genSym $ nm++"_delay"
                    insertAtEnd [Let (PatVar dnm UnspecifiedT) (SigDelay (Var nm) initE)]
                    return (Var dnm))
          undel e = return e
            
letFloating ::TravM ()
letFloating = mapDE letFl
    where letFl (LetE ses er) = do
            nns <- mapM genSym (map (unsafePatToName . fst) ses)
            let nonns = zip (map (unsafePatToName . fst) ses) nns
            nes <- forM (zip ses nns) $ \((PatVar n t,e), nn) -> do
                                  return $ [DeclareType nn t,
                                            Let (PatVar nn t) (changeVars nonns e)]
            insertBefore $ concat nes
            return (changeVars nonns er) 

          letFl e = return e

forgetFloating :: TravM ()
forgetFloating = mapDE forgetFl
    where forgetFl e@(Lam bd t arg) = return e -- cheap hack to fix above issue
          forgetFl (Forget tm se) = return Forget `ap` return tm `ap` mapEM forgetFloat se
          forgetFl e = mapEM forgetFloat e
          forgetFloat (Forget tm se) = do
            sn <- genSym "forgetFloat"
            insertBefore [Let (PatVar sn (EventT UnitT)) (Forget tm se)] --FIXME: not necessarily unit t
            return (Var sn)
          forgetFloat (e) = return e
          
sigFloating :: TravM () -- only sigFloat in values of type sig a. FIXME
sigFloating = mapDE sigFl
    where sigFl e@(Lam bd t arg) = return e -- cheap hack to fix above issue
          sigFl (Sig se) = Sig `fmap` mapEM sigFloat se
          sigFl e = mapEM sigFloat e
          sigFloat (Sig se) = do
            inSw <- insideSwitch
            inOde <- insideSolveOde
            hasBV <-  hasBoundVars se
            ep <- exprPath `fmap` get
            -- trace (concat ["considring sigfloat on ", pp se, ": ", show hasBV, show inSw, "\n"]) return ()
            -- trace (concat ["exprPath: ", intercalate ", " $ map pp ep, "\n"]) return ()
            if  hasBV || inSw || inOde
               then return (Sig se) {-do ln <- curLine
                       error $ "not sure what to do with bound var in sig floating in line \n"++ppDecl ln -}
               else {-if inSw
                       then return (Sig se)
                       else-} reallyFloatSig se
          sigFloat e = return e
          reallyFloatSig se = do 
            sn <- genSym "sigfl"
            insertBefore [Let (PatVar sn UnspecifiedT) (Sig se)]
            return (Var sn)


explicitSignalCopying :: TravM ()
explicitSignalCopying = mapDE explC
    where explS  e@(Var nm) = 
              do isoe <- isSignalOrEvt e
                 case isoe of
                   IsSig -> (return $ Sig (SigVal e))
                   _ -> return e 
          explC e = return e

renameCopiedEvents :: TravM ()
renameCopiedEvents = mapD rnmCE
    where rnmCE (Let (PatVar nm _) (Var nm')) = do renameEverywhere nm' nm -- ideally check which one
                                                   return Nop
          rnmCE d = return d 

removeNops :: TravM ()
removeNops = setter $ \s-> s{ decls = filter (not . isNop) $ decls s}
    where isNop Nop = True
          isNop _ = False

floatConnectedSignals :: TravM ()
floatConnectedSignals = mapD fCS
    where fCS e@(SinkConnect (Var nm) snm) = return e
          fCS e@(SinkConnect se (snm, arg)) = 
              do sn <- genSym snm
                 insertBefore [Let (PatVar sn (typeOfSrc snm)) se]
                 return (SinkConnect (Var sn) (snm,arg))
          fCS e = return e

evalIn :: [Declare] -> E -> V
evalIn decls e = unEvalM $ eval (evalS . evalManyAtOnce $ declsToEnv decls) e

evalSinkSrcArgs :: TravM () -- and signal limits, too
evalSinkSrcArgs = mapD eSSA
    where eSSA e@(SinkConnect se (snm, Const _)) = return e
          eSSA e@(SinkConnect se (snm, arg)) = do 
            --traceM "helo world"
            ds <- decls `fmap` get
            let carg =  evalIn ds arg
            --trace ("eval "++show arg++" to "++show carg) $ return ()
            return (SinkConnect se (snm, Const carg))
          eSSA e@(ReadSource vnm (snm, arg)) = do 
            --traceM "helo world"
            ds <- decls `fmap` get
            let carg =  evalIn ds arg
            --trace ("eval "++show arg++" to "++show carg) $ return ()
            return (ReadSource vnm (snm, Const carg))
          eSSA e = return e 

evalSigLimits :: TravM () -- and signal limits, too
evalSigLimits = mapD eSSA
    where eSSA (Let nm (SigLimited s lim )) = do
            ds <- decls `fmap` get
            let clim =  evalIn ds lim
            return (Let nm (SigLimited s (Const clim)))
          eSSA (Let nm (Forget lim e)) = do
            ds <- decls `fmap` get
            let clim =  evalIn ds lim
            return (Let nm (Forget (Const clim) e))
          eSSA e = return e 

substInferredTypesInPatterns :: TravM ()
substInferredTypesInPatterns = mapD sITIP
    where sITIP (Let (PatVar nm UnspecifiedT) e) = do
                 ds <- decls `fmap` get
                 case [t | DeclareType nm' t <- ds, nm == nm' ] of
                    t:_ -> return $ Let (PatVar nm t) e
                    [] -> return $ Let (PatVar nm UnspecifiedT) e
          sITIP d = return d

removePatDerivs :: TravM ()
removePatDerivs = mapD rTLPD >>  mapDE (mapEM rPD)
    where rTLPD (Let p@(PatDeriv _) e) =
                    let (nderivs, nm, tp) = procPat 0 p
                    in return $ (Let (PatVar nm tp) $ wrapSolves nderivs e)
          rTLPD e = return e
          rPD (LetE assocs le) = return $ LetE (map rPDLA assocs) le
          rPD e = return e
          rPDLA (p@(PatDeriv _), e) = let (nderivs, nm, tp) = procPat 0 p
                                  in (PatVar nm tp,  wrapSolves nderivs e)
          rPDLA assoc = assoc
          procPat n (PatDeriv p) = procPat (n+1) p
          procPat n (PatVar nm tp) = (n, nm, tp)
          wrapSolves 0 e = e
          wrapSolves n e = wrapSolves (n-1) $ SolveOde e

globalizeE :: String -> E -> TravM String
globalizeE s e = do gs <- genSym s
                    insertBefore [Let (PatVar gs UnspecifiedT) e]
                    return gs

floatSwitchEvents :: TravM ()
floatSwitchEvents = mapDE fSE
    where fSE (Switch ess er) = do ess' <- mapM rnmSE ess
                                   return $ Switch ess' er
          fSE e = return e
          rnmSE e@(Var en, lse) = return e
          rnmSE (ee, lse) = ifM (hasBoundVars ee)
                                (fail $ "bound vars in switch event")
                                (do en <- globalizeE "flE" ee
                                    return (Var en, lse))

connectsLast :: TravM ()
connectsLast = do ds <- decls `fmap` get 
                  let (ds2, ds1) = partition isConnect ds
                  setter $ \s-> s { decls = ds1++ds2 }
                  where isConnect (SinkConnect _ _) = True
                        isConnect _ = False

addStageAnnotations = whileChanges $ do
  ds <- decls `fmap` get
  let sAnnos = [ (nm, stage) | Stage nm stage <- ds ]
  let allSigs = [ nm | Let (PatVar nm _) _ <- filter declInMainLoop ds ]
  let existSnks = [n | SinkConnect (Var n1) (('#':n), _) <- ds, n1==n]
  let newSnks =[SinkConnect (Var nm) (('#':nm), Const Unit) | Stage nm _ <- ds, not $ nm `elem` existSnks  ]
  forM_ sAnnos $ \(nm, stage)-> do 
    defn <- lookUp nm 
    let depSigs = filter ((`isSubTermIn` defn) .  Var) $ allSigs
    forM_ depSigs $ \depSig-> do
      let sAnn =[ stage | Stage nmDs stageDs <- ds, 
                          nmDs==depSig, stageDs<=stage  ]
      when (null sAnn) $ insertAtEnd [Stage depSig stage]
  insertAtEnd newSnks

sigAtRefersToBuffer :: TravM ()
sigAtRefersToBuffer = mapDE $ \tle -> mapEM sAbuf tle
    where sAbuf e@(SigAt t (Var ('#':nm))) = return e
          sAbuf e@(SigAt t (Var nm)) =  return (SigAt t (Var $ '#':nm))
          sAbuf e = return e


--what if n2 is referred to?
simplifySomeLets :: TravM ()
simplifySomeLets = mapDE $ \tle -> mapEM sSL tle
    where sSL e@(LetE [(PatVar n1 t,e1)] (Var n2)) | n1 == n2 && (not $ Var n1 `isSubTermIn` e1)= return e1
                                                   | otherwise = return e
          sSL e = return e

massageDelayRefsInSwitch :: TravM ()
massageDelayRefsInSwitch = mapD mDRIS
    where mDRIS d@(Let gn (Switch mes le@(LetE [(PatVar n1 t,s1)] (Var n2)))) 
                           | n1 == n2  = return (Let gn $ Switch (mDris2 gn mes) $ mapE (sub n1 $ unsafePatToName gn) le)
                           | otherwise = return d 
          mDRIS d = return d
          mDris2 gn [] = []
          mDris2 gn ((ev, ls@(Lam tn _ (Lam vn _ (LetE [(PatVar n1 t1,s1)] (Var n2))))):tl) = (ev, mapE (sub n1 $ unsafePatToName gn) ls):mDris2 gn tl
          mDris2 gn (hd:tl) = hd:mDris2 gn tl
          sub sn tn e@(SigDelay (Var sn1) v0) | sn1 == sn = SigDelay (Var tn) v0
                                              | otherwise = e
          sub _ _ e = e

addBuffersToStore :: TravM ()
addBuffersToStore = mapD aBTS
    where aBTS sc@(SinkConnect (Var nm) ("store",_)) =  do
            --trace (show sc) $ return ()
            -- isSig <- isSignalOrEvt (Var nm)
            whenM ((==IsSig) `fmap` isSignalOrEvt (Var nm)) $ do
                    --trace ("foo!") $ return ()
                    insertAtEnd [SinkConnect (Var nm) (('#':nm), Const Unit)]
                
            return sc
          aBTS d = return d

declInMainLoop  (Let _ (Sig _)) = True
declInMainLoop  (Let _ (SigLimited _ _)) = True
declInMainLoop  (Let _ (Event _)) = True
declInMainLoop  (Let _ (ETest _ _)) = True
declInMainLoop  (Let _ (EScan _ _)) = True
declInMainLoop  (Let _ (Switch _ _)) = True
declInMainLoop  (Let _ (SigDelay _ _)) = True
declInMainLoop  (Let _ (SigFby _ _)) = True
declInMainLoop  (Let _ (Forget _ _)) = True
declInMainLoop  (Let _ (SolveOde _)) = True
declInMainLoop  (SinkConnect _ _) = True
declInMainLoop  (ReadSource _ _) = True
declInMainLoop  _ = False


inBoundVars :: String -> TravM Bool
inBoundVars nm = (nm `elem`) `fmap` boundVars `fmap` get

stripSig (Sig se) = se
stripSig (Event ee) = ee
stripSig e = e

data IsSigOrEvent = IsSig | IsEvt | IsNeitherSigNorEvt deriving (Show, Eq)

isSignalOrEvt :: E -> TravM IsSigOrEvent
isSignalOrEvt (Sig _) = return IsSig
isSignalOrEvt (SigDelay _ _) = return IsSig
isSignalOrEvt (Event _) = return IsEvt
isSignalOrEvt (ETest _ _) = return IsEvt
isSignalOrEvt (EScan _ _) = return IsEvt
isSignalOrEvt (Switch _ _) = return IsSig
isSignalOrEvt (SolveOde _) = return IsSig
isSignalOrEvt (Var "seconds") = return IsSig
isSignalOrEvt (Var "dt") = return IsNeitherSigNorEvt
isSignalOrEvt (Var nm) = ifM (isDefBySrc nm)
                             (do snm <- srcDefn nm
                                 return $ typeOfSrc' snm)
                             (ifM (isDefByDeriv nm) 
                                  (return IsSig)
                                  (do def <- lookUp nm
                                      isSignalOrEvt def))
isSignalOrEvt _ = return IsNeitherSigNorEvt

typeOfSrc' "adc" = IsSig
typeOfSrc' snm = case lookupSrc snm of
                 Just (Src _ _ (SignalT _) _ _) -> IsSig
                 Just (Src _ _ (EventT _) _ _) -> IsEvt
--                 Just (Src _ _ (ListT (PairT (PairT (NumT (Just RealT)) (NumT (Just RealT))) _)) _ _) -> IsEvt
                 Just (Src _ _ (ListT (PairT realT _)) _ _) -> IsEvt
                 Just (Src _ _ _ _ _) -> IsNeitherSigNorEvt
                 Nothing -> IsNeitherSigNorEvt

typeOfSrc "adc" = SignalT (NumT (Just RealT))
typeOfSrc snm = case lookupSrc snm of
                  Just (Src _ _ t _ _) -> t
                  Nothing -> error $ "can't find source "++snm

                 

hasBoundVars :: E-> TravM Bool
hasBoundVars e = or `fmap` queryM hasBvars e
    where hasBvars (Var nm) = (:[]) `fmap` inBoundVars nm
          hasBvars e = return [] 


hasSig :: E->TravM Bool
hasSig e = {- trace (pp e ) $ -} or `fmap` queryM (hasSigAux []) e
    where hasSigAux :: [String] -> E -> TravM [Bool]
          hasSigAux _ (Sig _) = return [True]
          hasSigAux _ (Event _) = return [True]
          hasSigAux _ (EScan _ _) = return [True]
          hasSigAux _ (ETest _ _) = return [True]
          hasSigAux _ (SigDelay _ _) = return [True]
          hasSigAux lu v@(Var nm) = 
              ifM ({-mor (inBoundVars nm) (isDefBySrc nm)) -} (dontLookUp nm)) 
                  (return [False])
                  $ do defn <- stripSig `fmap` lookUp nm
                       --pth <- exprPath `fmap` get
                       if v `isSubTermIn` defn ||  nm `elem` lu
                          then return [False] -- not sure about this but need to break loop
                          else  {-trace (nm++": "++pp defn) $-}  queryM (hasSigAux $ nm:lu) defn
          hasSigAux _ (_) = return [False] 

dontLookUp nm = or `fmap` sequence [ (inBoundVars nm),
                                       (isDefBySrc nm),
                                       (isDefByDeriv nm),
                                       (return ("#" `isPrefixOf` nm)),
                                       (return $ nm `elem` bivNms)
                                     ]

isDefByDeriv nm = do 
  ds <- decls `fmap` get
  return $ nm `elem` [nm' | Let (PatDeriv (PatVar nm' _)) _ <- ds]

--mor = liftM2 (||)

makesShape :: E->TravM Bool
makesShape e =  or `fmap` queryM (makesShapeAux []) e
    where makesShapeAux :: [String] -> E -> TravM [Bool]
          makesShapeAux _ (Box _) = return [True]
          makesShapeAux _ (Translate _ _) = return [True]
          makesShapeAux _ (Colour _ _) = return [True]
          makesShapeAux lu v@(Var nm) = 
              ifM (dontLookUp nm) 
                  (return [False])
                  $ do defn <-  lookUp nm
                       if v `isSubTermIn` defn ||  nm `elem` lu
                          then return [False] -- not sure about this but need to break loop
                          else queryM (makesShapeAux $ nm:lu) defn
          makesShapeAux _ (_) = return [False] 


isDefBySrc nm = do ds <- decls `fmap` get
                   return $ any test ds
    where test (ReadSource n _) = n == nm
          test _ = False

srcDefn nm = do ds <- decls `fmap` get
                return $ head [ snm | ReadSource n (snm,_) <- ds, n == nm ]
 
{-compilablePrelude :: TravM [Declare]
compilablePrelude = 
    do prel <- env `fmap` get
       compPrel <- filterM (\(n,e) -> ifM (hasSig e) (return False) (return True)) prel
       return $ map (\(n,e)->Let n e) compPrel -}
ack str = (traceM str >>)

transforms =   [(typeCheck, "typeCheck")
                ,(removePatDerivs, "removePatDerivs")
                ,(substInferredTypesInPatterns, "substInferredTypesInPatterns")
                ,(connectsLast, "connectsLast")
                ,(floatConnectedSignals, "floatConnectedSignals")
                ,(whileChanges substHasSigs, "substHasSigs")
                ,(betaRedHasSigs, "betaRedHasSigs")
                ,(substGetsSigs, "substGetsSigs")
                ,(whileChanges betaRedHasSigs, "betaRedHasSigs")
                ,(substMakesShape, "substMakesShape")
                ,(whileChanges betaRedMakesShape, "betaRedMakesShape")
                ,(letFloating, "letFloating")
                ,(sigFloating, "sigFloating")
                ,(forgetFloating, "sigFloating")
                ,(floatSwitchEvents, "floatSwitchEvents")
                ,(substHasSigs, "substHasSigs")
                ,(betaRedHasSigs , "betaRedHasSigs")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(recursiveSigGenApplication, "recursiveSigGenApplication")
                ,(massageDelayRefsInSwitch, "massageDelayRefsInSwitch")
                ,(explicitSignalCopying, "explicitSignalCopying")
                ,(renameCopiedEvents, "renameCopiedEvents")
                ,(removeNops, "removeNops")
                ,(evalSigLimits, "evalSigLimits")
--                ,(evalSinkSrcArgs, "evalSinkSrcArgs")
                ,(addStageAnnotations, "addStageAnnotations")
                --,(sigAtRefersToBuffer, "sigAtRefersToBuffer")
                ,(simplifySomeLets, "simplifySomeLets")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(addBuffersToStore, "addBuffersToStore")
               ]

transform :: TravM ()
transform = do
  sequence_ $ map (\(tr, nm) -> errorInfo ("in transform: "++nm) tr) transforms 
  --traceM "all trans Ok"
                

splitByStages :: [Declare] -> [[Declare]]
splitByStages ds = 
    let stages = nub [ s | Stage _ s <- ds ]
        (mainL, env) = partition declInMainLoop ds 
        stageDs st = let nms = [ nm | Stage nm s <- ds, s==st ]
                         in [ d | d@(Let (PatVar nm _) _) <- ds, nm `elem` nms ]++
                            [ d | d@(SinkConnect _ (('#':nm),_)) <- ds, nm `elem` nms]++
                            [ d | d@(ReadSource _ (nm,_)) <- ds, nm `elem` nms]
--                            [ d | d@(SinkConnect (Var nm) ("store",_)) <- ds, nm `elem` nms]
                            
        stagedDecls = map stageDs stages
        unstagedDecls = mainL \\ (concat stagedDecls)
    in (env : stagedDecls) ++ [unstagedDecls]


localTmax globaltmax decls =  let unLimSigs = [ nm | Let nm (Sig s) <- decls ]
                                  limSigs = [ lim | Let nm (SigLimited s (Const (NumV (NReal lim)))) <- decls ]
                              in if null unLimSigs && nonempty limSigs
                                    then maximum limSigs
                                    else globaltmax
