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
          stgLine (ds,n) = ind++expTuple (exports!!n)++ "goStage"++show n ++imps n
          imps n = impTuple $ accumExports (n-1)
          expTuple [] = ""
          expTuple nms = inTuple nms ++ " <- "
          impTuple [] = " ()"
          impTuple nms = " ("++intercalate "," nms++")"
          stages = concatMap comStageLine $ zip stgs [0..]
          comStageLine (ds,n) = compStage ds n (accumExports (n-1)) (exports!!n)

compStage ds n imps exps = ("goStage"++show n++" "++inTuple imps++" = do "):lns
    where ind = "   "
          loopInd = "       "
          sigs = [ (nm,e) | Let nm (Sig e) <- ds ]
          lns = initLns ++ initBuffers++ startLoop ++readSigs++writeSigs++returnBuffers++[""]
          startLoop = [ind++"forM_ [0..npnts-1] $ \\npt -> do", loopInd ++ "let secondsVal = npt*dt"]
          initLns = map (\sig-> ind++sig++" <- newIORef "++initVal sig) $ map fst sigs
          initVal sig = "undefined" -- lookup for initial val ?
          readSigs = map (\sig-> loopInd++sig++"Val <- readIORef "++sig) $ map fst sigs
          writeSigs = concatMap writeSig sigs
          writeSig (nm,e) | nm `elem` exps = [loopInd++"let "++nm++"CurVal = "++(pp $ tweakExpr e)
                                                     ,loopInd++"writeIORef "++nm++" "++nm++"CurVal"
                                                     ,loopInd++"appendIORef "++nm++"Buf "++nm++"CurVal"
                                                      ]
                                             
                          | otherwise = [loopInd++"writeIORef "++nm++" $ "++(pp $ tweakExpr e)]
          tweakExpr e = mapE (changeRead . unSharp . unVal) e 
          unVal (SigVal (Var nm)) = Var (nm++"Val")
          unVal e = e
          unSharp (Var ('#':nm)) = Var nm
          unSharp e = e
          changeRead (SigAt t s) = (App (App (Var "readSig") (t)) s)
          changeRead e = e
          initBuffers = map (\sig-> ind++sig++"Buf <- newIORef []") exps
          returnBuffers = [ind++"return "++(inTuple $ map (\nm-> "bufToSig "++nm) exps) ]

inTuple nms = "("++intercalate "," nms++")"

--stageExports ds = [ nm | SinkConnect (Var nm) ("store",_) <- ds]
stageExports ds = [ nm | SinkConnect (Var nm) ('#':_,_) <- ds]

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