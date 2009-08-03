module HaskellBackend where

import Statement
import Compiler
import Stages
import Expr
import PrettyPrint
import Traverse
import Control.Monad.State.Strict
import Transform 
import Types
import Data.Char
import Data.List

--forget about sinks, sources apart from store


--hide sum, map, snd, fst.
--translate, box?

--compileToHask :: String -> [Declare] -> IO String
compileToHask modNm dt tmax ds = do
  let prg = toHask modNm dt tmax ds
  writeFile (capitalize modNm++".hs") prg
  --putStrLn prg
  return prg

--toHask :: String -> [Declare] -> String
toHask modNm dt tmax ds 
    = let (env:stageDs) = splitByStages ds
          nonMainLoop = filter notbanned $ fst (runTravM env [] compilableEnv) 
      in unlines $ ["{-# LANGUAGE NoMonomorphismRestriction #-}",
                    "module "++capitalize modNm++" where",
                    "",
                    "import Numbers", 
                    "import HaskShapes",
                    "import Data.IORef",
                    "import Array",
                    "import Control.Monad","import EvalM hiding (tmax, dt)","",
                   "dt="++show dt, "tmax="++show tmax, "npnts="++show(round $ tmax/dt)]++
                   writeBufToSig++
                   map atTopLevel nonMainLoop++["", ""]++
                   compileStages stageDs++["", ""]++["{-"]++
                   concatMap (\ds-> "\na stage":map ppDecl ds) stageDs++["-}"]

--mapAccum :: (a->b) -> [a] -> [[b]]

writeBufToSig = ["", "bufToSig buf = ",
                         "   let arr = array (0,npnts) $ zip [0..npnts] $ reverse buf",
                         "   in Signal 0 tmax dt $ \\t-> arr!t", ""]


compileStages stgs =  "goMain = do ":lns++[ind++"return ()", ""]++stages
    where ind = "   " 
          lns = map stgLine $ zip stgs [0..]
          exports = map (nub . stageExports)  stgs
          accumExports n = concatMap (\i-> exports!!n) [0..n]
          stgLine (ds,n) = ind++expTuple ((exports!!n++eventExports ds) \\ imps n)++ "goStage"++show n ++" "++(inTuple $ imps n)
          imps n = accumExports (n-1)
          expTuple [] = ""
          expTuple nms = inTuple nms ++ " <- "
          inTuple [] = "()"
          inTuple nms = "("++intercalate "," nms++")"
          stages = concatMap comStageLine $ zip stgs [0..]
          comStageLine (ds,n) = compStage ds n (accumExports (n-1)) (exports!!n) ((eventExports ds \\ imps n) \\ (exports!!n))

compStage ds n imps exps evExps = ("goStage"++show n++" "++inTuple imps++" = do "):lns
    where ind = "   "
          loopInd = "       "
          sigs = [ (nm,e) | Let nm (Sig e) <- ds ]
          evs = [ nm | Let nm (Event e) <- ds ]
          comments = ["--imps="++show imps,
                      "--exps="++show exps,
                     "--evexps="++show evExps]
          lns = initLns ++ initBuffers++ startLoop ++readSigs++
                writeSigs++readSigBuffers++readEventBuffers++returnLine++[""]
          initLns = concatMap initLn ds
          initLn (Let nm (Sig e)) = [ind++nm++" <- newIORef undefined"]
          initLn (Let nm (Switch _ _)) = [ind++nm++" <- newIORef undefined"]
          initLn (Let nm (SigDelay (Var snm) v0)) = [ind++nm++" <- newIORef "++pp v0]
          initLn (Let nm (Event e)) = [ind++nm++"Queue <- newIORef []"]
          --initLn (Let nm (SigDelay (Var snm) v0)) = [ind++nm++" <- newIORef "++pp v0]
          initLn d = []
          initBuffers = map (\sig-> ind++sig++"Buf <- newIORef []") exps
          startLoop = [ind++"forM_ [0..npnts-1] $ \\npt -> do", loopInd ++ "let secondsVal = npt*dt"]
          --readSigs = map (\sig-> loopInd++sig++"Val <- readIORef "++sig) $ map fst sigs
          readSigs = concatMap readSig ds
          readSig (Let nm (Sig e)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let nm (Switch _ _)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let nm (SigDelay (Var snm) v0)) = [loopInd++nm++"Val <- readIORef "++nm]
          readSig (Let nm (Event _)) = [loopInd++nm++" <- readIORef "++nm++"Queue"]
          readSig d = []
          writeSigs = concatMap writeSig ds
          writeSig (Let nm (Sig e)) | nm `elem` exps = calcAndBuffer (nm, e)
                                    | otherwise = [loopInd++"let "++nm++"Val = "++(pp $ tweakExpr e)
                                                  ,loopInd++"writeIORef "++nm++" "++nm++"Val"]
          writeSig (Let nm (Event e)) = [loopInd++nm++" <- return $ ("++(pp $tweakExpr e)++")++"++nm, --SORT THESE!!!!
                                        loopInd++"writeIORef "++nm++"Queue "++nm]
          writeSig (Let nm s@(Switch ses se)) = switch nm ses se
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
          returnLine = [ind++"return "++(inTuple $ map (\nm-> "bufToSig "++nm++"BufVal") exps ++ map (++"QVal") evExps)]

tweakExpr e = mapE (changeRead . unSharp . unVal) e 
    where unVal (SigVal (Var nm)) = Var (nm++"Val")
          unVal e = e
          unSharp (Var ('#':nm)) = Var nm
          unSharp e = e
          changeRead (SigAt t s) = (App (App (Var "readSig") (s)) t)
          changeRead e = e

tweakEslam = tweakEslam' . tweakExpr

tweakEslam' (Lam t (Lam v (Sig s))) = (Lam t (Lam v s))
tweakEslam' (Lam t (Lam v (Var nm))) = (Lam t (Lam v (Var $ nm++"Val")))
tweakEslam' e = e

unSig (Sig s) = s
unSig e = e

test :: IO ()
test = do let xs = []
          xs <- return $ 1:xs -- xs refers to above, not recursive
          print $ take 5 xs -- [1]



inTuple nms = "("++intercalate "," nms++")"

--stageExports ds = [ nm | SinkConnect (Var nm) ("store",_) <- ds]
stageExports ds = [ nm | SinkConnect (Var nm) ('#':_,_) <- ds]

eventExports ds = [ nm | SinkConnect (Var nm) ("store",_) <- ds] \\ stageExports ds

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

notbanned (Let nm _) = not $ nm `elem` banned
notbanned (DeclareType nm _) = not $ nm `elem` banned

atTopLevel :: Declare -> String
atTopLevel (Let nm e) = nm ++ " = "++pp e
atTopLevel (DeclareType nm t) = nm ++ " :: "++haskTypeString t

banned = ["sum", "map", "fst", "snd", "max", "min"]

compilableEnv :: TravM [Declare]
compilableEnv =  do 
  ds <- (decls) `fmap` get
  cDs <- filterM filtf ds
  return $ cDs
      where filtf (Let nm e) = ifM (reactive e) (return False) (return True)
            filtf (DeclareType nm t) = do
                   lu <- safeLookUp nm
                   case lu of 
                     Nothing -> return False
                     Just defn -> filtf (Let nm defn)
            filtf _ = return False

reactive :: E->TravM Bool
reactive e = {- trace (pp e ) $ -} or `fmap` queryM (hasSigAux []) e
    where hasSigAux :: [String] -> E -> TravM [Bool]
          hasSigAux _ (Sig _) = return [True]
          hasSigAux _ (Event _) = return [True]
          hasSigAux _ (SigDelay _ _) = return [True]
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