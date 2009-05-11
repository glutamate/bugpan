module Transform where

import Expr
import Eval
--import Run 
import Control.Monad
import Numbers
import Data.Char 
import Data.List (partition, intercalate)
import Traverse
import Control.Monad.State.Strict
import Debug.Trace

substHasSigs :: TravM ()
substHasSigs = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            ifM (inBoundVars nm)
                (return e)
                $ do defn <- lookUp nm
                     ifM (hasSig $ stripSig defn)
                         (return $ App (defn) arg)
                         (return e) 
          subst e = return e

substGetsSigs :: TravM ()
substGetsSigs = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            ifM (inBoundVars nm)
                (return e)
                $ do defn <- lookUp nm
                     ifM (hasSig arg)
                         (return $ App (defn) arg)
                         (return e) 
          subst e = return e

substMakesShape :: TravM ()
substMakesShape = mapDE $ \tle -> mapEM subst tle
    where subst e@(App (Var nm) arg) = do
            ifM (inBoundVars nm)
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
betaContract eo@(App (Lam nm bd) arg) = mapE bar bd
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
                    insertAtEnd [Let dnm (SigDelay (Var nm) initE)]
                    return (Var dnm))
          undel e = return e
            
letFloating ::TravM ()
letFloating = mapDE letFl
    where letFl (LetE ses er) = do
            nns <- mapM genSym (map fst ses)
            let nonns = zip (map fst ses) nns
            nes <- forM (zip ses nns) $ \((n,e), nn) -> do
                                  return $ Let nn (changeVars nonns e)
            insertBefore nes
            return (changeVars nonns er) 

          letFl e = return e
                
sigFloating :: TravM () -- only sigFloat in values of type sig a. FIXME
sigFloating = mapDE sigFl
    where sigFl e@(Lam bd arg) = return e -- cheap hack to fix above issue
          sigFl (Sig se) = Sig `fmap` mapEM sigFloat se
          sigFl e = mapEM sigFloat e
          sigFloat (Sig se) = do
            inSw <- insideSwitch
            hasBV <-  hasBoundVars se
            ep <- exprPath `fmap` get
            -- trace (concat ["considring sigfloat on ", pp se, ": ", show hasBV, show inSw, "\n"]) return ()
            -- trace (concat ["exprPath: ", intercalate ", " $ map pp ep, "\n"]) return ()
            if  hasBV || inSw
               then return (Sig se) {-do ln <- curLine
                       error $ "not sure what to do with bound var in sig floating in line \n"++ppDecl ln -}
               else {-if inSw
                       then return (Sig se)
                       else-} reallyFloatSig se
          sigFloat e = return e
          reallyFloatSig se = do 
            sn <- genSym "sigfl"
            insertBefore [Let sn (Sig se)]
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
    where rnmCE (Let nm (Var nm')) = do renameEverywhere nm' nm -- ideally check which one
                                        return Nop
          rnmCE d = return d 

removeNops :: TravM ()
removeNops = setter $ \s-> s{ decls = filter (not . isNop) $ decls s}
    where isNop Nop = True
          isNop _ = False

floatConnectedSignals :: TravM ()
floatConnectedSignals = mapD fCS
    where fCS e@(SinkConnect (Var nm) snm) = return e
          fCS e@(SinkConnect se snm) = do sn <- genSym snm
                                          insertBefore [Let sn se]
                                          return (SinkConnect (Var sn) snm)
          fCS e = return e

globalizeE :: String -> E -> TravM String
globalizeE s e = do gs <- genSym s
                    insertBefore [Let gs e]
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
  let allSigs = [ nm | Let nm _ <- filter declInMainLoop ds ]
  let existSnks = [n | SinkConnect (Var n1) ('#':n) <- ds, n1==n]
  let newSnks =[SinkConnect (Var nm) ('#':nm) | Stage nm _ <- ds, not $ nm `elem` existSnks  ]
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
    where sSL e@(LetE [(n1,e1)] (Var n2)) | n1 == n2 && (not $ Var n1 `isSubTermIn` e1)= return e1
                                          | otherwise = return e
          sSL e = return e

massageDelayRefsInSwitch :: TravM ()
massageDelayRefsInSwitch = mapD mDRIS
    where mDRIS d@(Let gn (Switch mes le@(LetE [(n1, s1)] (Var n2)))) 
                           | n1 == n2  = return (Let gn $ Switch (mDris2 gn mes) $ mapE (sub n1 gn) le)
                           | otherwise = return d 
          mDRIS d = return d
          mDris2 gn [] = []
          mDris2 gn ((ev, ls@(Lam tn (Lam vn (LetE [(n1, s1)] (Var n2))))):tl) = (ev, mapE (sub n1 gn) ls):mDris2 gn tl
          mDris2 gn (hd:tl) = hd:mDris2 gn tl
          sub sn tn e@(SigDelay (Var sn1) v0) | sn1 == sn = SigDelay (Var tn) v0
                                              | otherwise = e
          sub _ _ e = e

addBuffersToStore :: TravM ()
addBuffersToStore = mapD aBTS
    where aBTS sc@(SinkConnect (Var nm) "store") =  do
            whenM ((==IsSig) `fmap` isSignalOrEvt (Var nm))
                $ insertAfter [SinkConnect (Var nm) ('#':nm)]
                
            return sc
          aBTS d = return d

declInMainLoop  (Let _ (Sig _)) = True
declInMainLoop  (Let _ (Event _)) = True
declInMainLoop  (Let _ (Switch _ _)) = True
declInMainLoop  (Let _ (SigDelay _ _)) = True
declInMainLoop  (SinkConnect _ _) = True
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
isSignalOrEvt (Switch _ _) = return IsSig
isSignalOrEvt (Var "seconds") = return IsSig
isSignalOrEvt (Var "dt") = return IsNeitherSigNorEvt
isSignalOrEvt (Var nm) = do def <- lookUp nm
                            isSignalOrEvt def

isSignalOrEvt _ = return IsNeitherSigNorEvt

hasBoundVars :: E-> TravM Bool
hasBoundVars e = or `fmap` queryM hasBvars e
    where hasBvars (Var nm) = (:[]) `fmap` inBoundVars nm
          hasBvars e = return [] 


hasSig :: E->TravM Bool
hasSig e = {- trace (pp e ) $ -} or `fmap` queryM (hasSigAux []) e
    where hasSigAux :: [String] -> E -> TravM [Bool]
          hasSigAux _ (Sig _) = return [True]
          hasSigAux _ (Event _) = return [True]
          hasSigAux _ (SigDelay _ _) = return [True]
          hasSigAux lu v@(Var nm) = 
              ifM (mor (inBoundVars nm) (isDefBySrc nm))
                  (return [False])
                  $ do defn <- stripSig `fmap` lookUp nm
                       --pth <- exprPath `fmap` get
                       if v `isSubTermIn` defn ||  nm `elem` lu
                          then return [False] -- not sure about this but need to break loop
                          else {- trace (nm++": "++pp defn) $ -} queryM (hasSigAux $ nm:lu) defn
          hasSigAux _ (_) = return [False] 

makesShape :: E->TravM Bool
makesShape e =  or `fmap` queryM (makesShapeAux []) e
    where makesShapeAux :: [String] -> E -> TravM [Bool]
          makesShapeAux _ (Box _) = return [True]
          makesShapeAux _ (Translate _ _) = return [True]
          makesShapeAux _ (Colour _ _) = return [True]
          makesShapeAux lu v@(Var nm) = 
              ifM (mor (inBoundVars nm) (isDefBySrc nm)) 
                  (return [False])
                  $ do defn <-  lookUp nm
                       if v `isSubTermIn` defn ||  nm `elem` lu
                          then return [False] -- not sure about this but need to break loop
                          else queryM (makesShapeAux $ nm:lu) defn
          makesShapeAux _ (_) = return [False] 

mor = liftM2 (||)

isDefBySrc nm = do ds <- decls `fmap` get
                   return $ any test ds
    where test (ReadSource n _) = n == nm
          test _ = False
 
compilablePrelude :: TravM [Declare]
compilablePrelude = 
    do prel <- env `fmap` get
       compPrel <- filterM (\(n,e) -> ifM (hasSig e) (return False) (return True)) prel
       return $ map (\(n,e)->Let n e) compPrel

transforms =   [(connectsLast, "connectsLast")
                ,(floatConnectedSignals, "floatConnectedSignals")
                ,(whileChanges substHasSigs, "substHasSigs")
                ,(betaRedHasSigs, "betaRedHasSigs")
                ,(substGetsSigs, "substGetsSigs")
                ,(whileChanges betaRedHasSigs, "betaRedHasSigs")
                ,(substMakesShape, "substMakesShape")
                ,(whileChanges betaRedMakesShape, "betaRedMakesShape")
                ,(letFloating, "letFloating")
                ,(sigFloating, "sigFloating")
                ,(floatSwitchEvents, "floatSwitchEvents")
                ,(substHasSigs, "substHasSigs")
                ,(betaRedHasSigs , "betaRedHasSigs")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(massageDelayRefsInSwitch, "massageDelayRefsInSwitch")
                ,(explicitSignalCopying, "explicitSignalCopying")
                ,(renameCopiedEvents, "renameCopiedEvents")
                ,(removeNops, "removeNops")
                ,(addStageAnnotations, "addStageAnnotations")
                ,(sigAtRefersToBuffer, "sigAtRefersToBuffer")
                ,(simplifySomeLets, "simplifySomeLets")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(sigFloating, "sigFloating")
                ,(unDelays, "unDelays")
                ,(addBuffersToStore, "addBuffersToStore")
               ]

transform :: TravM ()
transform = sequence_ $ map fst transforms
                
