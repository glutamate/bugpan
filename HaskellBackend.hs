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
--forget about sinks, sources apart from store


--hide sum, map, snd, fst.
--translate, box?

--compileToHask :: String -> [Declare] -> IO String
test = do ds <- fileDecls "Intfire" []
          compileToHask "Intfire.hs" 0.001 0.2 ds []

getDeclaredType :: [Declare] -> String -> T
getDeclaredType ds nm = head [ t | DeclareType nm' t <- ds, nm == nm']

compileToHask fp dt tmax ds params = do
  --putStrLn "hello"
  let prg = toHask dt tmax ds params
  writeFile (fp) prg
  --putStrLn prg
  return prg

--toHask :: String -> [Declare] -> String
toHask dt tmax ds params
    = let (env:stageDs) = splitByStages ds
          nonMainLoop = filter (notbanned (map fst params) ) $
                        filter (notbanned banned) $ fst (runTravM env [] compilableEnv) 
      in unlines $ ["{-# LANGUAGE NoMonomorphismRestriction, BangPatterns#-}",
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
                    "floor = realToFrac . P.floor ","",
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

rootDir = "/var/bugpan/sessions"

mainFun ds exps = 
    let ind = "   "
        modNm = head [nm | Let (PatVar "moduleName" _) (Const (StringV nm)) <- ds]
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


writeSelSwitch = ["selSwitch :: [([(Double, a)], Double -> a -> b)] -> b -> b",
                  "selSwitch eslams def = selSwitch' eslams def 0"]


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


catMayMap f xs = catMaybes . map f $ xs

compStageP ds' tmax n imps exps evExps = ("goStage"++show n++" "++inTuple imps++" = do "):lns
    where ind = "   "
          ds = snd $ runTravM ds' [] removeSigLimits
          dsSrcs =  [(varNm, fromJust $ lookupSrc srcNm,param) | ReadSource varNm (srcNm, param) <- ds]
          atOnceSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcOnce srcf), param)<- dsSrcs ]
          rtSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcRealTimeSig srcf), param)<- dsSrcs ]
          --sigs = ("seconds", (App (Var "realToFrac") (Var "npnts"-Var"n"))/Var "dt"):[ (nm,e) | Let (PatVar nm _) (Sig e) <- ds ]
          --evs = [ nm | Let nm (Event e) <- ds ]
          lns = newTmax++runAtOnceSrcs++ defineStep++runLine++retLine
          newTmax = let tm = localTmax tmax ds' in
                    [ind++"let tmax = "++(show $ tm),
                    ind++"let npnts = idInt . round $ tmax/dt"]
          runLine = [ind++"("++intercalate "," (exps++evExps)++") <- return $ step1 npnts "++(intercalate " " $ initVls)]
          retLine = [ind++"return ("++(intercalate "," $ map ("bufToSig tmax "++) exps++ evExps)++")\n"]
          --returnLine = [ind++"return "++(inTuple $ map (\nm-> "bufToSig tmax "++nm++"BufVal") exps ++ map (++"QVal") evExps)]
          expBufs = map (++"Buf") exps
          consExpBufs = map (\nm-> (nm++"Buf", Cons (Var nm) (Var (nm++"Buf")))) exps

          arg (Let (PatVar nm _) (Sig e)) = Just nm
          arg (Let (PatVar nm _) (Event e)) = Just nm
          arg (Let (PatVar nm _) (SigDelay (Var snm) v0)) = Just nm
          arg (Let (PatVar nm _) (Switch eslams s0)) = Just nm
          arg (Let (PatVar nm _) (Forget tm se)) = Just nm
          arg (Let (PatVar nm _) (SolveOde (SigFby v e))) = Just $ nm
          arg s = Nothing
          bufArgs = map (++"Buf") exps
          args = ["seconds"]++catMaybes (map arg ds)++bufArgs
          
          initV (Let nm (Sig e)) = Just "undefined"
          initV (Let nm (Event e)) = Just "[]"
          initV (Let nm (Forget tm e)) = Just $ pp e
          initV (Let nm (SigDelay (Var snm) v0)) = Just $ pp v0
          initV (Let nm (Switch eslams s0)) = Just "undefined"
          initV (Let nm (SolveOde (SigFby v e))) = Just $ pp v
          initV s = Nothing
          bufInits = map (\_->"[]") exps
          initVls =["0"]++catMaybes (map initV ds)++bufInits

          switchLine nmv ((Var nm), esLam) = "("++nm++", "++(pp $ (tweakEslamP (nmOrd nm) ds) esLam)++")"

          callE (Let (PatVar nm _) _) = Just $ nm++"NxtV"
{-          callE (Let nm (Event e)) = Just $ "("++(pp . tweakExprP $ e) ++")++"++nm
          callE (Let nm (SigDelay (Var snm) v0)) = Just $ snm
          callE (Let nm (Switch ses s0)) = Just $ "selSwitch ["++(intercalate "," $ map switchLine ses)++"] ("++(pp . unSig $ tweakExprP s0)++")" -}
          callE s = Nothing
          bufCallEs = map (\nm-> nm++"NxtV:"++nm++"Buf") exps
          callEs = ("secondsNxtV"):catMaybes (map callE ds)++bufCallEs
          
          nmOrd = nmOrderinDS ds

          lets (Let (PatVar nm _) (Sig e)) = Just (nm++"NxtV", pp . (tweakExprP (nmOrd nm) ds) $ e)
          lets (Let (PatVar nm _) (SigDelay (Var snm) _)) = Just (nm++"NxtV",snm++"NxtV" )
          lets (Let (PatVar nm _) (Event e)) = 
                                  Just $ (nm++"NxtV", "("++(pp . (tweakExprP (nmOrd nm) ds) $ e) ++")++"++nm)
          lets (Let (PatVar nm _) (Forget (Const tmV) se)) = 
                        Just $ (nm++"NxtV", "dropWhile ((<(seconds-"++ppVal tmV++")) . fst) "++nm)
          lets (Let (PatVar nm _) (Switch ses s0)) = 
                                  Just (nm++"NxtV",  "selSwitch ["++(intercalate "," $ map (switchLine nm) ses)++
                                                     "] ("++(pp . unSig $ (tweakExprP (nmOrd nm) ds) s0)++")")
          lets (Let (PatVar nm _) (SolveOde (SigFby v e))) = Just (nm++"NxtV",nm++"+ dt*("++(pp . unSig . (tweakExprP (nmOrd nm) ds) $ e)++")")
          lets e = Nothing

          letss = ("secondsNxtV", "realToFrac (npnts- n)*dt"):catMaybes (map lets ds)

          breakStep =  "\n                         " 
          runAtOnceSrcs = map (\(v,s,p)-> ind++v++" <- "++s++" tmax dt "++pp p) atOnceSrcs 
          --initVls = ["0"]++map (\_->"undefined") (init sigs)++ (map (inPar . pp . (tweakExprP (nmOrd nm) ds) .snd) delays)++map (\_->"[]") exps
          defineStep = [ind++"let step 0 "++(intercalate " " args) ++" = ("++
                          (intercalate ", " $ expBufs++evExps )++")",
                        ind++"    step !n "++(intercalate " "  $ map ('!':) args)++" = \n"++replicate 20 ' '++
                           " let {\n" ++(concatMap (\(nm,expr)->nm ++ "="++expr++";\n") letss)++"} in "++
                          "step (n-1) "++breakStep++(intercalate breakStep $ map (inPar) callEs),
                        ind++"    step1 !n "++(intercalate " " args)++" = \n"++replicate 20 ' '++
                           " let {\n" ++(concatMap (\(nm,expr)->nm ++ "="++expr++";\n") letss)++"} in "++
                          "step (n-1) "++breakStep++(intercalate breakStep $ map (inPar) callEs) ]

bang nm = '!':nm

isEvent :: String -> [Declare] -> Bool
isEvent nm ds = not $ null [() | Let (PatVar nm' _) (Event e) <- ds, nm' ==nm]

nmOrderinDS ds nm = aux 0 ds 
    where aux n [] = error $ "nmOrderinDS: can't find "++nm
          aux n ((Let (PatVar nm' _) _):dss) | nm' == nm = n
                                  | otherwise = aux (n+1) dss
          aux n (d:dss) = aux (n+1) dss

tweakExprP n ds e = mapE (changeRead . unSharp . unVal) e 
    where unVal (SigVal (Var ('#':nm))) = Var (nm) -- ++"Val")
          unVal (SigVal (Var "seconds")) = Var "secondsNxtV"
          unVal (SigVal (Var nm)) | nmOrderinDS ds nm < n = Var (nm++"NxtV") -- ++"Val")
                                  | otherwise = Var nm
          unVal (Var nm) | isEvent nm ds && nmOrderinDS ds nm < n = Var (nm++"NxtV") -- ++"Val")
                         | otherwise = Var nm
          unVal e = e
          unSharp (Var ('#':nm)) = Var nm
          unSharp e = e
          changeRead (SigAt t s) = (App (App (Var "readSig") (s)) t)
          changeRead e = e

tweakEslamP n ds = tweakEslamP' . tweakExprP n ds

tweakEslamP' (Lam t tt (Lam v vt (Sig s))) = (Lam t tt (Lam v vt s))
--tweakEslamP' (Lam t tt (Lam v vt (SolveOde s))) = (Lam t tt (Lam v vt ()))
--tweakEslamP' (Lam t tt (Lam v vt (Var nm))) = (Lam t tt (Lam v vt (Var $ nm++"Val")))
tweakEslamP' e = e

inPar s = "("++s++")"



{-compStage ds' tmax n imps exps evExps = ("goStage"++show n++" "++inTuple imps++" = do "):lns
    where ind = "   "
          loopInd = "       "
          ds = snd $ runTravM ds' [] removeSigLimits
          dsSrcs =  [(varNm, fromJust $ lookupSrc srcNm,param) | ReadSource varNm (srcNm, param) <- ds]
          atOnceSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcOnce srcf), param)<- dsSrcs ]
          rtSrcs = [(varNm, srcf, param) | (varNm, Src _ _ _ _ (SrcRealTimeSig srcf), param)<- dsSrcs ]
          sigs = [ (nm,e) | Let  (PatVar nm _) (Sig e) <- ds ]
          evs = [ nm | Let (PatVar nm _) (Event e) <- ds ]
          comments = ["--imps="++show imps,
                      "--exps="++show exps,
                     "--evexps="++show evExps]
          lns = initLns ++ newTmax ++ initBuffers++ runAtOnceSrcs++ startLoop ++readSigs++
                writeSigs++readSigBuffers++readEventBuffers++returnLine++[""]
          initLns = concatMap initLn ds
          initLn (Let (PatVar nm _) (Sig e)) = [ind++nm++" <- newIORef undefined"]
          initLn (Let (PatVar nm _) (Switch _ _)) = [ind++nm++" <- newIORef undefined"]
          initLn (Let (PatVar nm _) (SigDelay (Var snm) v0)) = [ind++nm++" <- newIORef "++pp v0]
          initLn (Let (PatVar nm _) (Event e)) = [ind++nm++"Queue <- newIORef []"]
          initLn d = []
          debugDsSrcs = show dsSrcs
          runAtOnceSrcs = ("{-"++debugDsSrcs++"-}"):map (\(v,s,p)-> ind++v++" <- "++s++" tmax dt "++pp p) atOnceSrcs 
          newTmax = let tm = localTmax tmax ds' in
                    [ind++"let tmax = "++(show $ tm),
                    ind++"let npnts = idInt . round $ tmax/dt"]
          initBuffers = map (\sig-> ind++sig++"Buf <- newIORef []") exps
          startLoop = [ind++"forM_ [0..npnts-1] $ \\npt -> do", 
                       loopInd ++ "let secondsVal = (realToFrac npt)*dt"]
          --readSigs = map (\sig-> loopInd++sig++"Val <- readIORef "++sig) $ map fst sigs
          readSigs = concatMap readSig ds
          readSig (Let (PatVar nm _) (Sig e)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let (PatVar nm _) (Switch _ _)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let (PatVar nm _) (SigDelay (Var snm) v0)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let (PatVar nm _) (Event _)) = [loopInd++nm++" <- readIORef "++nm++"Queue"]
          readSig d = []
          writeSigs = concatMap writeSig ds
          writeSig (Let (PatVar nm _) (Sig e)) | nm `elem` exps = calcAndBuffer (nm, e)
                                               | otherwise = [loopInd++"let "++nm++"Val = "++(pp $ tweakExpr e)
                                                  ,loopInd++"writeIORef "++nm++" "++nm++"Val"]
          writeSig (Let pv (SigDelay (Var snm) _)) = writeSig (Let pv (Sig $ SigVal (Var snm)))
          writeSig (Let (PatVar nm _) (Event e)) = [loopInd++nm++" <- return $ ("++(pp $tweakExpr e)++")++"++nm, --SORT THESE!!!!
                                        loopInd++"writeIORef "++nm++"Queue "++nm]
          writeSig (Let (PatVar nm t) s@(Switch ses se)) = switch nm ses se
          writeSig d = []
          switch nm ses se = [loopInd++"let "++nm++"Switch = ["++
                              (intercalate "," $ map switchLine ses)++"]",
                             loopInd++"let "++nm++"Val = selSwitch "++nm++"Switch ("++
                             (pp . unSig $ tweakExpr se)++")"
                             ,loopInd++"writeIORef "++nm++" "++nm++"Val"
                             ]++maybeExp nm
          maybeExp nm = (if nm `elem` exps then [loopInd++"appendIORef "++nm++"Buf "++nm++"Val"] else [])
          switchLine ((Var nm), esLam) = "("++nm++", "++(pp $ tweakEslam esLam)++")"
          calcAndBuffer (nm, e) = [loopInd++"let "++nm++"Val = "++(pp  $ tweakExpr e)
                                  ,loopInd++"writeIORef "++nm++" "++nm++"Val"
                                  ,loopInd++"appendIORef "++nm++"Buf "++nm++"Val"
                                  ]
          readSigBuffers = map (\nm->ind++nm++"BufVal <- readIORef "++nm++"Buf") exps
          readEventBuffers = map evQ evExps
          evQ nm | nm `elem` evs = ind++nm++"QVal <- readIORef "++nm++"Queue"
                 | otherwise = ind++"let "++nm++"QVal = "++nm
          returnLine = [ind++"return "++(inTuple $ map (\nm-> "bufToSig tmax "++nm++"BufVal") exps ++ map (++"QVal") evExps)]
-}


tweakExpr e = mapE (changeRead . unSharp . unVal) e 
    where unVal (SigVal (Var nm)) = Var (nm++"Val")
          unVal e = e
          unSharp (Var ('#':nm)) = Var nm
          unSharp e = e
          changeRead (SigAt t s) = (App (App (Var "readSig") (s)) t)
          changeRead e = e

tweakEslam = tweakEslam' . tweakExpr

tweakEslam' (Lam t tt (Lam v vt (Sig s))) = (Lam t tt (Lam v vt s))
tweakEslam' (Lam t tt (Lam v vt (Var nm))) = (Lam t tt (Lam v vt (Var $ nm++"Val")))
tweakEslam' e = e

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

