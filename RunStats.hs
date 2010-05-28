module Main where

import Parse
import Expr
import System.Environment
import Database
import Traverse
import EvalM as EM
import Eval
import Numbers 
import Control.Monad
import Control.Monad.State.Lazy
import TNUtils
import Math.Probably.Sampler
import Math.Probably.Distribution
import Query (bugpanRootDir)
import ReactiveDistributions
import Query
import QueryTypes
import Data.List
import Data.Maybe
import Stats.Simulate
import Types
import PrettyPrint
--ideas:
-- exclamation mark for saving values in simfakes 

help = putStrLn $ unlines [
        "runstats simfakes {model file} {session base} {session count}",
        "runstats estimate {model file} {session filter}\n"
        ]       

main = getArgs >>= dispatch

unConst (Const e) = e 

dispatch ("simfakes":filenm:sessBase:_) = do
  ds <- fileDecls filenm []
  --mapM print ds
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  fakeenv <- fmap (EM.env . head) $ runSamplerIO $ evalSim es ds
  --mapM print $ fakeenv
  saveEnv sessBase tmax dt $ fakeenv
  return ()

dispatch ("estimate":filenm:rest) = do
  ds <- fileDecls filenm []
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  let rvs = fillSigs tmax dt $ rvars ds
  let filtr = case rest of
                   [] -> "[()]"
                   s:_ -> s
  --mapM print rvs
                          
  let prog =  
       unlines [initialStuff,
                ppParamTypes rvs,

                ppUpdaters [] rvs rvs,

                mlEstimators rvs,
                thetmaxdt tmax dt, 

                loadData filtr rvs,

                theGibbs rvs,
                themain,
                initialise rvs,
                ppOfInterest rvs] 

  writeFile ("Estimator.hs") prog
  --sh "ghc --make -O2 Estimator"

  return ()

dispatch _ = help


thetmaxdt tmax dt = "thetmax = "++show tmax ++"\nthedt="++show dt++"\n\n"


data RVar = RVar String T E
          | InEvery String [RVar]
          deriving Show

mapEinRVars :: (E->E) -> [RVar] -> [RVar]
mapEinRVars f = map g
    where g (RVar nm t e) = RVar (nm) t (mapE f e) 
          g (InEvery nm rvs) = InEvery nm $ mapEinRVars f rvs

--fillSig :: Storable a => Double -> Double -> Double -> (Double -> a) -> Signal a
fillSigs :: Double -> Double -> [RVar] -> [RVar]
fillSigs tmax dt allrvs = mapEinRVars f allrvs
  where f (Sig se) = {- Var "fillSig" $> 0 $> Const (pack tmax) $> 
                        Const (pack dt) $> -} Lam "seconds" UnspecifiedT (mapE g se)
        f e = e
        g (SigVal (Var "seconds")) = Var "seconds"
        g e = e
        


rvarName (RVar nm _ _) = nm

flattenRVars :: [RVar] -> [RVar]
flattenRVars [] = []
flattenRVars (r@(RVar _ _ _):rs) = r : flattenRVars rs
flattenRVars (r@(InEvery _ subs):rs) = r : (flattenRVars subs ++ flattenRVars rs)

rvars :: [Declare] -> [RVar]
rvars = rvar []
   where rvar env ((Distribute p e'):ds) = 
             let e = subMany env e' in
             RVar (unsafePatToNameWithBang p) (typeOfDist e) e : rvar env ds
         rvar env ((Every p dure decls):ds) = 
             (InEvery (durExprToName dure) $ rvar env decls) : rvar env ds
         rvar env ((Let p e):ds) = rvar ((unsafePatToName p, e):env) ds
         rvar env [] = []

typeOfDist (App (App (Var "N") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "uniform") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "RandomSignal") _) _) = SignalT $ NumT (Just RealT)
typeOfDist (App (Var "unknown") _) = NumT (Just RealT)
typeOfDist e = error $ "typeOfDist: unknown expr" ++ show e

durExprToName (Var nm) = nm
durExprToName (App (Var nm) _) = nm

ppParamTypes :: [RVar] -> String
ppParamTypes rs = 
    let topPars = [ppSubParStype ("Par"++nm) rvs | InEvery nm rvs <- flattenRVars rs]
    in ppSubParStype "AllPars" rs++concat topPars

ppSubParStype :: String -> [RVar] -> String
ppSubParStype nm rvs = "data "++nm++" = "++nm++" {\n"++(intercalate ",\n" $ map pprv rvs)++"} deriving Show\n\n"
    where ind = replicate 4 ' '
          pprv (RVar vnm t _ ) | hasExclaim vnm =  ind ++noExclaim vnm++" :: "++haskTypeString t
                               | otherwise =  ind ++vnm++" :: Param "++haskTypeString t
          pprv (InEvery dnm rs) =  ind ++ dnm++" :: [Par"++dnm++"]\n"

ppUpdaters :: [String] -> [RVar]  -> [RVar] -> String
ppUpdaters _ _ [] = ""
ppUpdaters outerPath allrvs rs@((InEvery durnm rvs):rest) = 
    ppUpdaters (durnm:outerPath) allrvs rvs ++ ppUpdaters outerPath allrvs rest
ppUpdaters outerPath allrvs ((RVar vnm t ex):rest) = 
    if (hasExclaim vnm) then "" else alllines 
    where alllines = unlines [thetype, updName++" allPars = do",
             ind++"new"++vnm++" <- "++smany++ppSamplerE vnm parpath dists, {-" metSampleP "++show vnm++" (\\"++vnm++" -> ",
--             intercalate "+\n" $ map (((ind++ind)++) . pp) dists,
             ind++ind++pp (simplifyDist vnm $ foldl1 (+) dists),
             ind++ind++") $ "++parpath, -} -- vnm++" $ " ++concatMap (++" $") outerPath ++" allpars",
             ind++"return $ "++retval++"\n" -- children: "++show (childrenOf vnm allrvs)++"\n"
            ] ++ ppUpdaters outerPath allrvs rest
          updName = "update_"++(concat $ map (++"_") outerPath)++vnm
          thetype = "update_"++(concat $ map (++"_") outerPath)++vnm++" :: AllPars -> Sampler AllPars"
          smany = case outerPath of
                    [] -> "" -- for2 (session allpars) running
                    [p] -> "sampleMany $ for ("++p++" allPars) $ \\"++p++"-> "
                    [p1,p2] -> "sampleMany2 $ for2' (" ++p2++" allPars) "++p1++" $ \\"++p2++" "++p1++"-> "
          retval = case outerPath of
                    [] -> "allPars { "++vnm++" = new"++vnm++" }"
                    [p] -> "allPars { "++p++" = for (zip ("++p++" allPars) new"++vnm++") $ \\(s,n) -> s { "++vnm++" = n } }"
                    [p2, p1] -> "allPars { "++p1++" = for (zip ("++p1++" allPars) new"++vnm++") $ \\(s,nvs) -> s { "++p2++" = for (zip ("++p2++" s) nvs) $ \\(r,n) -> r { "++vnm ++" = n } } }"
          ind = replicate 4 ' '
          ppath myout cnm = (pathE $ pathOf myout cnm allrvs)
          distSum :: [String] -> [String] -> E
          distSum myout [cnm] = unPex myout [vnm] allrvs $ distE $ childDist cnm
          distSum myout (cnm:cnms) | cnm `elem` myout = distSum (myout) cnms
                                   | otherwise = Var "sum" $> 
                                       ((Var "for" $> ppath myout cnm) $> 
                                       Lam cnm UnspecifiedT (distSum (cnm:myout) cnms))
          
          childDist cnm = ($>(Var cnm)) $ lookupDist allrvs cnm
          lh = distE (ex $> Var vnm)
          parpath = pathE $ pathOf outerPath vnm allrvs
          dists = unPex outerPath [vnm] allrvs lh : (map (distSum outerPath) $ childrenOf vnm allrvs)

distE (App (App (Var "unknown") _) _)= 1
--distE (App (App (Var "N") mue) sde) = Var "P.logGaussD" $> mue $> sde
distE (App (App (App (Var "N") mue) sde) xe) = Var "P.logGaussD" $> mue $> sde $> xe
distE (App (App (App (Var "uniform") loe) hie) xe) = Var "P.uniform" $> loe $> hie $> xe
distE (App (App (App (Var "RandomSignal") wfe) noisee) sige) = Var "pdf" $> (Var "RandomSignalFast" $> wfe $> noisee) $> sige
distE d = error $ "distE: "++show d
lookupDist :: [RVar] -> String -> E
lookupDist rvars nm = head $ [e | RVar nm' t e <- flattenRVars rvars, nm' ==nm]

simplifyDist v (M2 Add e1 e2) | isConstWrt v e1 = simplifyDist v e2
                              | isConstWrt v e2 = simplifyDist v e1
                              | otherwise = M2 Add (simplifyDist v e1) (simplifyDist v e2)
simplifyDist v e = e

isConstWrt nm (Const _) = True
isConstWrt nm (M1 _ e) =  isConstWrt nm e
isConstWrt nm (M2 _ e1 e2) =  isConstWrt nm e1 && isConstWrt nm e2
isConstWrt nm (Var vnm)= nm /= vnm 
isConstWrt nm _ = False

ppSamplerE vnm path dists = pp $ samplerE vnm path $ simplifyDist vnm $ sum dists
samplerE vnm path distE = Var "metSampleP" $> Const (StringV vnm) $> Lam vnm UnspecifiedT distE $> path

pathE :: [String] -> E
pathE = pathE' . reverse
        where pathE' [x] = Var x 
              pathE' (x:xs) = Var x $> (pathE' xs)
              pathE' [] = Nil

takeWhilePlusOne _ [] = []
takeWhilePlusOne p (x:xs) | p x = x : takeWhilePlusOne p xs 
                          | otherwise = x:[]

pathOf :: [String] -> String -> [RVar] -> [String]
pathOf outerPath nm rvs = case pathOf' nm rvs of
                            (x:_) -> blockOuter outerPath $ "allPars":x
                            [] -> error $ "can't find path to "++nm
    where --blockOuter [] fullPath = [] -- fullPath
          blockOuter outer full =  if any (`elem` outer) full
                                      then {-[head outer] ++ -} (reverse $ takeWhilePlusOne (not . (`elem` outer)) $ reverse full) 
                                      else full

pathOf' :: String -> [RVar] -> [[String]]
pathOf' nm rvs = [[noExclaim nm'] | RVar nm' t e <- rvs, nm==nm']++ -- first attempt
                 [[dnm] | InEvery dnm moreRvars <- rvs, dnm == nm]++
                 concat [map (dnm:) $ pathOf' nm moreRvars | InEvery dnm moreRvars <- rvs]

allLevels rvs =  [[dnm] | InEvery dnm moreRvars <- rvs]++
                 concat [map (dnm:) $ allLevels moreRvars | InEvery dnm moreRvars <- rvs]
allExclaim rvs =  [[r] | r@(RVar nm _ _) <- rvs, hasExclaim nm]++
                  concat [map (ie:) $ allExclaim moreRvars | ie@(InEvery dnm moreRvars) <- rvs]
allNoExclaim rvs =  [[r] | r@(RVar nm _ _) <- rvs, not $ hasExclaim nm]++
                    concat [map (ie:) $ allNoExclaim moreRvars | ie@(InEvery dnm moreRvars) <- rvs]

inLevel nm rvs = 
   concat [mrvs | InEvery dnm mrvs <- flattenRVars rvs, dnm == nm ]

mlEstimators allrvs = concatMap mlSubEstimator (allLevels allrvs) ++ mlGlobalEstimator allrvs

mlSubEstimator path = "mlEstimator_"++last path++" allpars = do\n  return ()\n"

mlGlobalEstimator path = "mlGlobalEstimator allpars = do\n  return ()\n"

loadData filtr allrvs =           
   unlines ["loadData :: IO AllPars",
            "loadData = do",
            ind++"sessVls <- manySessionData $ do",
            ind2++"running <- durations \"running\" ()",
            ind2++"modNm <- durations \"moduleName\" \"foo\"",
            ind2++"sess <- sessionDur",
            ind2++"liftIO $ putStrLn $ snd $ head sess"]++
   (concatMap showGetter $ allExclaim allrvs)++
   unlines [ind2++"let runners = for running $ \\r-> "++getRunner,
            ind2++"let sessVal = Parsession "++
               concatMap (mkval "") (inLevel "session" allrvs)++" runners",
            ind2++"if not $ null $ "++filtr,
            ind2++"   then return $ Just sessVal",
            ind2++"   else return Nothing", 
            ind++"return $ AllPars "++mksess ++"sessVls"
            ]
   where showGetter nms = 
            let RVar nm t e = last nms 
                
            in ind2++noExclaim nm++" <- "++getter t++" "++show (noExclaim nm)++ " "++ proxy t++"\n"
         proxy (NumT (Just RealT)) = "(1::Double)"
         proxy (ListT (PairT (NumT (Just RealT)) t)) = proxy t
         proxy (UnitT) = "()"
         proxy (SignalT (NumT (Just RealT))) = ""
         getter (SignalT (NumT (Just RealT))) = "signalsDirect"
         getter (ListT (PairT (PairT (NumT (Just RealT)) 
                              (NumT (Just RealT))) t)) = "durations"
         getter (ListT (PairT (NumT (Just RealT)) t)) = "events"
         getter t = "durations"
         ind = replicate 3 ' '   
         ind2 = ind++ind 
         ind3 = ind2++ind 
         mksess = intercalate " " [ "NonInitialisedParam " | 
                                    RVar nm _ _ <-allrvs, not $ hasExclaim nm ]
         getRunner = "Parrunning "++concatMap (mkval "during [r]") (inLevel "running" allrvs)
         mkval during (RVar nm t e) | hasExclaim nm && getter t == "durations" 
                                        = "(snd $ head $ "++during++" "++noExclaim nm++") "
                                    | hasExclaim nm && getter t == "signalsDirect" 
                                        = "(sigZero $ head $ "++during++" "++noExclaim nm++") "
                                    | hasExclaim nm = "("++during++" "++noExclaim nm++") "
                                    | otherwise = "NonInitialisedParam "
         mkval _ _ = ""
                       

initialStuff = 
  unlines ["module Main where",
           "import EvalM",
           "import System.Environment",
           "import Data.List",
           "import Data.Maybe",
           "import TNUtils",
           "import Query ",
           "import Control.Monad",
           "import Math.Probably.Sampler",
           "import Math.Probably.Distribution",
           "import Math.Probably.StochFun",
           "import Math.Probably.MCMC",
           "import qualified Math.Probably.PDF as P",
           "import QueryTypes",
           "import Math.Probably.FoldingStats",
           "import PlotGnuplot",
           "import QueryPlots",
           "import QueryUtils hiding (groupBy)",
           "import Database",
           "import Data.Array.Vector",
           "import qualified Data.Vector.Unboxed as U",
           "import Data.Binary",
           "import StatsModel",
           "import ReactiveDistributions",
           "import Foreign.Storable",
           "import Foreign.C",
           "import Foreign.ForeignPtr",
           "import Foreign.Ptr",
           "import Control.Arrow",
           "import FitGnuplot",
           "import NewSignal",
           "import Locust"]

theGibbs allrvs = ("thegibbs = "++) $ intercalate " >-> " $ map f $ allNoExclaim allrvs
         where f path = let RVar nm t e = last path
                            outerPath = reverse [dnm | InEvery dnm rvs <- path]
                        in "update_"++(concat $ map (++"_") outerPath)++nm

themain = 
   unlines ["main = do",
            "  countStr:filenm:_ <- getArgs",
            "  writeFile (filenm++\"_parnames.mcmc\") $ show parNames", 
            "  justData <- fmap initialise loadData",
            "  let baymarkov = Mrkv (condSampler thegibbs) (justData) id",
            "  ps <- take (read countStr) `fmap` runMarkovIO baymarkov",
            "  writeInChunks (filenm++\"_chain0\") 20000 $ map ofInterest ps",
            --"  mapM print $ zip parNames $ ofInterest (last ps)",
            "  return ()",
            "  ---writeFile (filenm++\"_parnames.mcmc\") $ show parNames "]


initialise allrvs = 
   unlines ["initialise :: AllPars -> AllPars",
            "initialise justData = AllPars "++par [] ["justData"] allrvs]
  where par env path ((RVar vnm t e):rest) | hasExclaim vnm = 
                                               "("++ppath (noExclaim vnm:path)++") "++ par env path rest
                                           | otherwise = let v = distToInit env e in
                                                         "(newParam "++(ppVal v)++") "++ 
                                                         par ((vnm, v):env) path rest
        par env path ((InEvery durnm rvs):rest) = "(for ("++ppath (durnm:path)++") $ \\"++durnm++"-> Par"++ durnm ++" "++par env (durnm:path) rvs++")"++par env path rest
        par _ _ [] = ""
        ppath path = intercalate "$" $ take 2 path

eval' env e = unEvalM $ eval (extsEnv env emptyEvalS) e

distToInit env (App (Var "unknown") e)=  eval' env e 
distToInit env (App (App (Var "N") me) se) = eval' env me
distToInit env (App (App (Var "uniform") loe) hie) = eval' env $ (loe+hie)/2

{-last2 [] = []
last2 [x] = [x]
last2 [x,y] = [x,y]
last2 (x:xs) = last2 xs -}

nmsFromRvars rvs = [vnm | RVar vnm _ _ <- rvs]

ppOfInterest rvs =
    let tlvars = nmsFromRvars rvs
        tlvarsPath = map (\vnm-> "unP $ "++vnm++" allPars") tlvars
        sessvarNms = filter (not . hasExclaim) $ nmsFromRvars $ concat $ [svars | InEvery "session" svars <- rvs]
        trialvarNms = filter (not . hasExclaim) $ nmsFromRvars $ concat $ [trvars | InEvery "session" svars <- rvs, 
                                                        InEvery "running" trvars <- svars]
        sessvarsPath = map (\vnm-> "unP $ "++vnm++" $ head $ session allPars") sessvarNms
        trialvarsPath = map (\vnm-> "unP $ "++vnm++" $ head $ running $ head $ session allPars") trialvarNms
        withSessId = map (++"s0")
        withTrId = map (++"s0tr0")
    in "ofInterest allPars = ["++ intercalate ", " (tlvarsPath++sessvarsPath++trialvarsPath)++
                   "]\nparNames = "++show (tlvars++withSessId sessvarNms++withTrId trialvarNms)

childrenOf :: String -> [RVar] -> [[String]]
childrenOf nm rvs = 
    [[nm'] | RVar nm' t e <- rvs, Var nm `elem` flatE e]++ -- first attempt
    concat [map (dnm:) $ childrenOf nm moreRvars | InEvery dnm moreRvars <- rvs]
unPex outer notThese rvs = mapE f
        where f (Var nm) | hasExclaim nm = pathE $ pathOf outer nm rvs
                         | nm `elem` [vnm | RVar vnm _ _ <- flattenRVars rvs] &&
                           not (nm `elem` notThese) 
                             = (App (Var "unP") $ pathE $ pathOf outer nm rvs)
                         | otherwise = Var nm
              f e = e
              --thecomment nm = "{-"++show (outer, pathOf' nm rvs)++"-}" 


{-
plan :

-stupid initialiser
-run it!
-ofInterest
-ML estimator
-}