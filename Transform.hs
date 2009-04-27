module Transform where

import Expr
import Eval
import Run
import Control.Monad
import Numbers
import Data.Char 
import Data.List (partition)
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


betaRedHasSigs :: TravM ()
betaRedHasSigs = mapDE $ \tle -> mapEM brhs tle
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
unDelays = mapDE unDelays' 
    where unDelays' e@(SigDelay (Var _) _) = return e
          unDelays' tle = mapEM undel tle
          undel (SigDelay (Var nm) initE) = do
            --assumes nm not bound and inite has no bound vars. FIXME
            dnm <- genSym $ nm++"_delay"
            insertAtEnd [Let dnm (SigDelay (Var nm) initE)]
            return (Var dnm)
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
                
sigFloating :: TravM ()
sigFloating = mapDE sigFl
    where sigFl (Sig se) = Sig `fmap` mapEM sigFloat se
          sigFl e = mapEM sigFloat e
          sigFloat (Sig se) = do
            inSw <- insideSwitch
            hasBV <-  hasBoundVars se
            if hasBV && not inSw
               then do ln <- curLine
                       error $ "not sure what to do with bound var in sig floating in line \n"++ppDecl ln
               else if inSw
                       then return (Sig se)
                       else reallyFloatSig se
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
  forM_ sAnnos $ \(nm, stage)-> do 
    defn <- lookUp nm
    let depSigs = filter ((`isSubTermIn` defn) .  Var) $ allSigs
    forM_ depSigs $ \depSig-> do
      let sAnn =[ stage | Stage nmDs stageDs <- ds, 
                          nmDs==depSig, stageDs<=stage  ]
      when (null sAnn) $ insertAtEnd [Stage depSig stage]

declInMainLoop  (Let _ (Sig _)) = True
declInMainLoop  (Let _ (Event _)) = True
declInMainLoop  (Let _ (Switch _ _)) = True
declInMainLoop  _ = False


transform :: TravM ()
transform = do  connectsLast
                floatConnectedSignals
                whileChanges substHasSigs  
                betaRedHasSigs 
                substGetsSigs
                whileChanges betaRedHasSigs 
                letFloating 
                sigFloating 
                floatSwitchEvents
                unDelays
                explicitSignalCopying
                renameCopiedEvents
                removeNops
                addStageAnnotations
                

inBoundVars :: String -> TravM Bool
inBoundVars nm = (nm `elem`) `fmap` boundVars `fmap` get

stripSig (Sig se) = se
stripSig (Event ee) = ee
stripSig e = e

data IsSigOrEvent = IsSig | IsEvt | IsNeitherSigNorEvt

isSignalOrEvt :: E -> TravM IsSigOrEvent
isSignalOrEvt (Sig _) = return IsSig
isSignalOrEvt (SigDelay _ _) = return IsSig
isSignalOrEvt (Event _) = return IsEvt
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
hasSig e = or `fmap` queryM hasSigAux e
    where hasSigAux :: E -> TravM [Bool]
          hasSigAux (Sig _) = return [True]
          hasSigAux (Event _) = return [True]
          hasSigAux (Var nm) = ifM (inBoundVars nm)
                                   (return [False])
                                   $ do defn <- stripSig `fmap` lookUp nm
                                        queryM hasSigAux defn
          hasSigAux (_) = return [False] 
 
compilablePrelude :: TravM [Declare]
compilablePrelude = 
    do prel <- env `fmap` get
       compPrel <- filterM (\(n,e) -> ifM (hasSig e) (return False) (return True)) prel
       return $ map (\(n,e)->Let n e) compPrel