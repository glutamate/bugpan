module HaskellBackend where

import Statement
--import Compiler
--import Stages
import Expr
import PrettyPrint
import Traverse
import Control.Monad.State.Strict
import Transform 
import Types
import Data.Char
import Data.List
import CompiledSrcsSinks
import Parse
import Data.Maybe
import TNUtils
import EvalM
import Query (bugpanRootDir)
--forget about sinks, sources apart from store


--hide sum, map, snd, fst.
--translate, box?

--compileToHask :: String -> [Declare] -> IO String
test = do ds <- fileDecls "Intfire" []
          compileToHask "Intfire.hs" 0.001 0.2 ds []

getDeclaredType :: [Declare] -> String -> T
getDeclaredType ds nm = 
    case [ t | DeclareType nm' t <- ds, nm == nm'] of
         x:_ -> x
         _ -> error $ "getDeclaredType: cant find "++nm
                 

compileToHask fp dt tmax ds params = do
  --forM_ ds $ putStrLn .ppDecl
  
  putStrLn $ "compToHask params: "++show params
  let prg = toHask dt tmax ds params
  writeFile (fp) prg
  --putStrLn prg
  return prg

--toHask :: String -> [Declare] -> String
toHask dt tmax ds params
    = let (env:stageDs) = splitByStages ds
          nonMainLoop = filter (notbanned (map fst params) ) $
                        filter (notbanned banned) $ fst (runTravM env [] compilableEnv) 
      in unlines $ ["{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}",
                    --"module "++capitalize modNm++" where",
                    "module Main where",
                    "",
                    "import Prelude hiding (floor)",
                    "import qualified Prelude as P",
                    --"import Numbers", 
                    "import HaskShapes",
                    "import Data.IORef",
                    "import System.Environment",
                    "import System.Time",
                    "import Array",
                    "import TNUtils",
                    "import NewSignal",
                    "import Database",
                    "import Control.Monad",
                    "import System.IO.Unsafe",

                    "import EvalM hiding (tmax, dt)"]++
                   allSrcImports ds++
                   ["",
                    "default (Int, Double)",
                    "floor = realToFrac . P.floor ","", "dt::Double", "tmax :: Double",
                   "dt="++show dt, "tmax="++show tmax, "npnts="++show(round $ tmax/dt)]++
                   writeBufToSig++writeSelSwitch++
                   concatMap readParam (zip params [2..])++
                   map atTopLevel nonMainLoop++["", ""]++
                   compileStages ds tmax stageDs++["", ""]++ ["{-"]++
                   concatMap (\ds-> "\na stage":map ppDecl ds) stageDs++["\nenv\n:"]++
                   map ppDecl env++ ["-}"]

--mapAccum :: (a->b) -> [a] -> [[b]]

readParam ((nm,ty), idx) = [nm++" = unsafePerformIO $ do",
                            "   args <- getArgs",
                            "   return ((read (args!!"++show idx++"))::"++haskTypeString ty++")"]

allSrcImports ds = let snms = [nm | ReadSource _ (nm, _) <- ds]
                   in concatMap (\s-> map ("import "++) $ srcImpModule s) . catMaybes $ map lookupSrc snms

rootDir = bugpanRootDir ./ "sessions"

headErr _ (x:_) = x
headErr s _ = error s

mainFun ds exps = 
    let ind = "   "
        modNm = headErr "no module defined" 
                 [nm | Let (PatVar "moduleName" _) (Const (StringV nm)) <- ds]
    in
          ["main = do",
           ind++"sessNm:t0Str:_ <- getArgs",
           ind++"sess <- loadApproxSession \""++rootDir++"\" sessNm", 
           ind++"tnow <- getClockTime",
           ind++"let t0 = read t0Str",
           ind++inTuple exps++" <- goMain",
           concatMap (\nm->ind++"saveInSession sess \""++nm++"\" t0 dt $ pack ("++nm++"::"++(haskTypeString $ getDeclaredType ds nm)++")\n") exps,
           ind++"saveInSession sess \"tStart\" t0 dt $ pack ([(0,())]::[(Double,())])\n",
           ind++"saveInSession sess \"tStop\" t0 dt $ pack ([(tmax,())]::[(Double,())])\n",
           ind++"saveInSession sess \"running\" t0 dt $ pack ([((0,tmax),())]::[((Double,Double),())])\n",
           ind++"saveInSession sess \"moduleName\" t0 dt $ pack ([((0,tmax),\""++unCap modNm++"\")]::[((Double,Double),String)])\n",
           ind++"return ()",


           ""
          ]

writeBufToSig = ["", 
                 "bufToSig tmax buf = ",
--                 "   let np = idInt . round $ tmax/dt",
--                 "       arr = array (0,np) $ zip [0..np] $ reverse buf",
--                 "       sf p = if p>np-1 then arr!(np-1) else if p<0 then arr!0 else arr!p",
                 " sigRevArr $ listToSig dt 0 buf"]

--Signal 0 tmax dt sf Eq", ""]


writeSelSwitch = ["selSwitch :: Double -> [([(Double, a)], Double -> a -> b)] -> b -> b",
                  "selSwitch t eslams def = selSwitch' t eslams def 0"]


compileStages ds tmax stgs =  mainFun ds allStore ++ ["goMain = do "] ++lns++retLine++stages
    where ind = "   " 
          retLine = [ind++"return "++inTuple allStore, ""]
          lns = map stgLine $ zip stgs [0..]
          allDs = concat stgs
          allStore = [ nm | SinkConnect (Var nm) ("store",_) <- allDs]
          exports = map (nub . stageExports)  stgs
          accumExports n = concatMap (\i-> exports!!n) [0..n]
          stgLine (ds,n) = ind++expTuple ((exports!!n++eventExports ds) \\ imps n)++ "goStage"++show n ++" "++(inTuple $ imps n)
          imps n = accumExports (n-1)
          expTuple [] = ""
          expTuple nms = inTuple nms ++ " <- "
          stages = concatMap comStageLine $ zip stgs [0..]
          comStageLine (ds,n) = compStageP ds tmax n (accumExports (n-1)) (exports!!n) ((eventExports ds \\ imps n) \\ (exports!!n))

removeSigLimits :: TravM ()
removeSigLimits = mapD rSL
    where rSL (Let nm (SigLimited e _)) = return (Let nm (Sig e))
          rSL d = return d

foo :: IO ()
foo = do 
  let f 0 =1
      f 1 = 0


  return ()

--lookup tp see that it is not realtime
--isEventFullyDefined ds nm = not. null $ [() | ReadSource varNm (srcNm, arg) <- ds, varNm == nm ]

comment s = ("--"++s++"\n")


catMayMap f xs = catMaybes . map f $ xs

compStageP ds' tmax n imps exps evExps = ("goStage"++show n++" "++inTuple imps++" = do "):lns
    where ind = "   "
          ds = snd $ runTravM ds' [] removeSigLimits
          dsSrcs =  [(varNm, fromJust $ lookupSrc srcNm,param) | ReadSource varNm (srcNm, param) <- ds]
          atOnceSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcOnce srcf), param)<- dsSrcs ]
          rtSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcRealTimeSig srcf), param)<- dsSrcs ]
          --sigs = ("seconds", (App (Var "realToFrac") (Var "npnts"-Var"n"))/Var "dt"):[ (nm,e) | Let (PatVar nm _) (Sig e) <- ds ]
          --evs = [ nm | Let nm (Event e) <- ds ]
          lns = newTmax++runAtOnceSrcs++ defineStep++runLine++retLine++["--rtsrcs: "]++map (comment . show) rtSrcs
          newTmax = let tm = localTmax tmax ds' in
                    [ind++"let tmax = "++show tm,
                     ind++"let npnts = idInt . round $ tmax/dt"]
          runLine = [ind++"("++intercalate "," (exps++evExps)++") <- step1 npnts "++(intercalate " " $ initVls)]
          retLine = [ind++"return ("++(intercalate "," $ map ("bufToSig tmax "++) exps++ evExps)++")\n"]
          --returnLine = [ind++"return "++(inTuple $ map (\nm-> "bufToSig tmax "++nm++"BufVal") exps ++ map (++"QVal") evExps)]
          expBufs = map (++"Buf") exps
          consExpBufs = map (\nm-> (nm++"Buf", Cons (Var nm) (Var (nm++"Buf")))) exps

--we are going to assume that all escans work on events that have been
--created in the last timestep. This is inefficient and incorrect (in
--case of sequential escans). In the future, transform escans so they
--always take a named event, and change events (etest or escan) to gen
--two let vars: one for new events

          arg (Let (PatVar nm _) (Sig e)) = [nm]
          arg (Let (PatVar nm t) (SigFby v e)) =  arg (Let (PatVar nm t) e)
          arg (Let (PatVar nm _) (Event e)) = [nm]
          arg (Let (PatVar nm _) (ETest e e')) = [nm, nm++"HappenedLast"]
          arg (Let (PatVar nm _) (EScan e e')) = [nm]
          arg (Let (PatVar nm _) (SigDelay (Var snm) v0)) = [nm]
          arg (Let (PatVar nm _) (Switch eslams s0)) = [nm]
          arg (Let (PatVar nm _) (Forget tm se)) = [nm]
          arg (Let (PatVar nm _) (SolveOde (SigFby v e))) = [nm]
          arg s = []
          bufArgs = map (++"Buf") exps
          args = ["seconds"]++concat (map arg ds)++bufArgs

          initV (Let nm (Sig e)) = ["undefined"]
          initV (Let nm (Event e)) = ["[]"]
          initV (Let nm (EScan _ _)) = ["[]"]
          initV (Let nm (ETest e1 e2)) = ["[]", "True"]
          initV (Let nm (Forget tm e)) = [ pp e]
          initV (Let nm (SigDelay (Var snm) v0)) = [ pp v0]
          initV (Let nm (Switch eslams s0)) = initV (Let nm s0)
          initV (Let nm (SigFby v e)) = [ pp v]
          initV (Let nm (SolveOde e)) = let inner = initV (Let nm e)
                                        in if null inner then ["undefined"] else inner
          initV (Let nm (LetE [(PatVar nm'' t, e)] (Var nm'))) 
                         | nm' == nm'' = initV (Let nm e)
                         | otherwise = []
          initV s = []
          bufInits = map (\_->"[]") exps
          initVls =["0"]++concat (map initV ds)++bufInits

          switchLine nmv ((Var nm), esLam) = "("++nm++", "++(pp $ (tweakEslamP nmv (nmOrd nm) ds) esLam)++")"

          callE (Let (PatVar nm _) (ETest _ _)) = [nm++"NxtV", nm++"HappenedLastNxt"]
          callE (Let (PatVar nm _) _) = [nm++"NxtV"]
{-          callE (Let nm (Event e)) = Just $ "("++(pp . tweakExprP $ e) ++")++"++nm
          callE (Let nm (SigDelay (Var snm) v0)) = Just $ snm
          callE (Let nm (Switch ses s0)) = Just $ "selSwitch ["++(intercalate "," $ map switchLine ses)++"] ("++(pp . unSig $ tweakExprP s0)++")" -}
          callE s = []
          bufCallEs = map (\nm-> nm++"NxtV:"++nm++"Buf") exps
          callEs = ("secondsNxtV"):concat (map callE ds)++bufCallEs
          
          nmOrd = nmOrderinDS ds

          lets (Let (PatVar nm _) (Sig e)) =  [(nm++"NxtV", pp . deepUnSig . (tweakExprP (nmOrd nm) ds) $ e)]
          lets (Let (PatVar nm t) (SigFby _ e)) = lets (Let (PatVar nm t) e)   
          lets (Let (PatVar nm _) (SigDelay (Var snm) _)) =  [(nm++"NxtV",snm++"NxtV" )]
          lets (Let (PatVar nm _) (Event e)) = 
                                  [(nm++"NxtV", "("++(pp . (tweakExprP (nmOrd nm) ds) $ e) ++")++"++nm)]
          lets (Let (PatVar nm _) (EScan f e)) =  let pp' = pp . (tweakExprP (nmOrd nm) ds) in
                                  [(nm++"NxtV", "(concatMap ("++(pp' f) ++") $ takeWhile ((>(seconds-dt/2)) . fst) $ "++pp' e++")++"++nm)]
          lets (Let (PatVar nm _) (ETest pe se)) = let pp' = pp . (tweakExprP (nmOrd nm) ds) 
                                                       predTrue = "("++pp' pe++") sv" in
                                  [("("++nm++"NxtV, "++nm++"HappenedLastNxt)", 
                                    "let sv = "++pp' (SigVal se)++" in if "++nm++"HappenedLast then ("++nm++
                                    ", "++predTrue++
                                    ") else (if "++predTrue++" then (((seconds,sv):"++nm++
                                    "), True) else ("++nm++", False))")]
          lets (Let (PatVar nm _) (Forget (Const tmV) se)) = 
                        [(nm++"NxtV", "dropWhile ((<(seconds-"++ppVal tmV++")) . fst) "++nm)]
          lets (Let (PatVar nm _) (Switch ses s0)) = 
                                  [(nm++"NxtV",  "selSwitch seconds ["++(intercalate "," $ map (switchLine nm) ses)++
                                                     "] ("++(pp $ (tweakEslamP nm (nmOrd nm) ds) s0)++")")]
          lets (Let (PatVar nm _) (SolveOde (SigFby v e))) = [(nm++"NxtV",nm++"+ dt*("++(pp . unSig . (tweakExprP (nmOrd nm) ds) $ e)++")")]
          lets e = []
          rtSrcLets = map (\(nm,_,_)->(nm++"NxtV", nm)) rtSrcs

          letss = (("secondsNxtV", "realToFrac (npnts- n)*dt"):concat (map lets ds))++rtSrcLets

          breakStep =  "\n                         " 
          runRtSrcs = map (\(v,s,p)-> ind++v++" <- "++s++" tmax dt "++pp p) rtSrcs
          runAtOnceSrcs = map (\(v,s,p)-> ind++v++" <- "++s++" tmax dt "++pp p) atOnceSrcs 
          --initVls = ["0"]++map (\_->"undefined") (init sigs)++ (map (inPar . pp . (tweakExprP (nmOrd nm) ds) .snd) delays)++map (\_->"[]") exps
          defineStep = [ind++"let step 0 "++(intercalate " " args) ++" = return ("++
                          (intercalate ", " $ expBufs++evExps )++")",
                        ind++"    step !n "++(intercalate " "  $ map ('!':) args)++" = do\n"
                           ++concatMap (\s->replicate 18 ' '++s++"\n") runRtSrcs
                           ++replicate 20 ' '++
                           " let {\n" ++(concatMap (\(nm,expr)->nm ++ "="++expr++";\n") letss)++"} in "++
                          "step (n-1) "++breakStep++(intercalate breakStep $ map (inPar) callEs),
                        ind++"    step1 !n "++(intercalate " " args)++" = do \n"
                           ++concatMap (\s->replicate 18 ' '++s++"\n") runRtSrcs
                           ++replicate 20 ' '++
                           " let {\n" ++(concatMap (\(nm,expr)->nm ++ "="++expr++";\n") letss)++"} in "++
                          "step (n-1) "++breakStep++(intercalate breakStep $ map (inPar) callEs) ]

bang nm = '!':nm

isEvent :: String -> [Declare] -> Bool
isEvent nm ds = not $ null $ [() | Let (PatVar nm' _) (Event e) <- ds, nm' ==nm]++
                             [() | Let (PatVar nm' _) (EScan _ _) <- ds, nm' ==nm]++
                             [() | Let (PatVar nm' _) (ETest _ _) <- ds, nm' ==nm]

nmOrderinDS ds nm = aux 0 ds 
    where aux n [] = Nothing --error $ "nmOrderinDS: can't find "++nm
          aux n ((Let (PatVar nm' _) _):dss) | nm' == nm = Just n
                                  | otherwise = aux (n+1) dss
          aux n (d:dss) = aux (n+1) dss

tweakExprP n ds e = mapE (changeRead . unSharp . unVal) e 
    where unVal (SigVal (Var ('#':nm))) = Var (nm) -- ++"Val")
          unVal (SigVal (Var "seconds")) = Var "secondsNxtV"
          unVal (SigVal (Var nm)) | isJust (nmOrderinDS ds nm) && isJust n && 
                                    fromJust (nmOrderinDS ds nm) < fromJust n 
                                      = Var (nm++"NxtV") -- ++"Val")
                                  | otherwise = Var nm
          unVal (Var nm) | isEvent nm ds && isJust (nmOrderinDS ds nm) && 
                           isJust n && fromJust (nmOrderinDS ds nm) < fromJust n 
                             = Var (nm++"NxtV") -- ++"Val")
                         | otherwise = Var nm
          unVal e = e

unSharp (Var ('#':nm)) = Var nm
unSharp e = e
changeRead (SigAt t s) = (App (App (Var "readSig") (s)) t)
changeRead e = e

--this stuff should really be a transform.
tweakEslamP nm n ds = deepUnSig . tweakEslamP' nm . tweakExprP n ds

tweakEslamP' nm (Lam t tt (Lam v vt e)) = (Lam t tt (Lam v vt (tweakEslamP' nm e)))
tweakEslamP' nm (Sig se) = tweakEslamP' nm se
tweakEslamP' nm e@(LetE [(PatVar nm'' t, SolveOde ode)] (Var nm')) 
    | nm' == nm'' = tweakEslamP' nm. SolveOde $ subVar nm' (Var nm) ode 
    | otherwise = e 
tweakEslamP' nm e@(SolveOde (SigFby _ ode)) = tweakEslamP' nm (SolveOde ode)
tweakEslamP' nm e@(SolveOde ode) = Var nm + Var "dt"*ode
--tweakEslamP' (Lam t tt (Lam v vt (SolveOde s))) = (Lam t tt (Lam v vt ()))
--tweakEslamP' (Lam t tt (Lam v vt (Var nm))) = (Lam t tt (Lam v vt (Var $ nm++"Val")))
tweakEslamP' _ e = e

inPar s = "("++s++")"

--unSolveOde (SolveOde


{-tweakExpr e = mapE (changeRead . unSharp . unVal) e 
    where unVal (SigVal (Var nm)) = Var (nm++"Val")
          unVal e = e
          unSharp (Var ('#':nm)) = Var nm
          unSharp e = e
          changeRead (SigAt t s) = (App (App (Var "readSig") (s)) t)
          changeRead e = e -}

{-tweakEslam = tweakEslam' . tweakExpr

tweakEslam' (Lam t tt (Lam v vt (Sig s))) = (Lam t tt (Lam v vt s))
tweakEslam' (Lam t tt (Lam v vt (Var nm))) = (Lam t tt (Lam v vt (Var $ nm++"Val")))
tweakEslam' e = e -}

deepUnSig = mapE unSig
   

unSig (Sig s) = s
unSig e = e

test2 :: IO ()
test2= do let xs = []
          xs <- return $ 1:xs -- xs refers to above, not recursive
          print $ take 5 xs -- [1]



inTuple nms = "("++intercalate "," nms++")"

--stageExports ds = [ nm | SinkConnect (Var nm) ("store",_) <- ds]
stageExports ds = [ nm | SinkConnect (Var nm) ('#':_,_) <- ds]

eventExports ds = [ nm | SinkConnect (Var nm) ("store",_) <- ds] \\ stageExports ds

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

notbanned banned (Let (PatVar nm t) _) = not $ nm `elem` banned
notbanned banned (DeclareType nm _) = not $ nm `elem` banned

atTopLevel :: Declare -> String
atTopLevel (Let (PatVar nm t) e) = nm ++ " = "++pp e
atTopLevel (DeclareType nm t) = nm ++ " :: "++haskTypeString t

banned = ["sum", "map", "fst", "snd", "max", "min"]

compilableEnv :: TravM [Declare]
compilableEnv =  do 
  ds <- (decls) `fmap` get
  cDs <- filterM filtf ds
  return $ cDs
      where filtf (Let _ e) = ifM (reactive e) (return False) (return True)
            filtf (DeclareType nm t) = do
                   lu <- safeLookUp nm
                   case lu of 
                     Nothing -> return False
                     Just defn -> filtf (Let (PatVar nm t) defn)
            filtf _ = return False

reactive :: E->TravM Bool
reactive e = {- trace (pp e ) $ -} or `fmap` queryM (hasSigAux []) e
    where hasSigAux :: [String] -> E -> TravM [Bool]
          hasSigAux _ (Sig _) = return [True]
          hasSigAux _ (Event _) = return [True]
          hasSigAux _ (EScan _ _) = return [True]
          hasSigAux _ (ETest _ _) = return [True]
          hasSigAux _ (SigDelay _ _) = return [True]
          hasSigAux _ (SolveOde _) = return [True]
          hasSigAux _ (SigVal _) = return [True]
          hasSigAux _ (SigAt _ _) = return [True]
          hasSigAux lu v@(Var nm) = 
              ifM ({-mor (inBoundVars nm) (isDefBySrc nm)) -} (dontLookUp nm))
                  (return [False])
                  $ do mdefn <- safeLookUp nm
                       --pth <- exprPath `fmap` get
                       case mdefn of 
                         Nothing -> return [True]
                         Just defn ->  if v `isSubTermIn` defn ||  nm `elem` lu
                                          then return [False] -- not sure about this but need to break loop 
                                          else  {-trace (nm++": "++pp defn) $-}  queryM (hasSigAux $ nm:lu) defn
          hasSigAux _ (_) = return [False] 

transDecls :: TravM a -> [Declare] -> [Declare]
transDecls trans ds = let runTM = runTravM ds [] in snd . runTM $ trans

-- Local Variables:
-- compile-command: "make runbugpan && ./RunBugpan -o Intfire.hs Intfire"
-- End:

