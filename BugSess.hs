module Main where

import Database
import Query
import System.Environment
import TNUtils
import EvalM
import System.Directory
import Traverse (ifM)
import Control.Monad
import Control.Monad.Writer.Lazy
import Language.Haskell.Interpreter
import QueryTypes
import Data.Char
--import System.Posix.Resource
import ValueIO
import PrettyPrint
import Numbers
import System.Cmd
import Format2
import Data.List
import System.Time
import Data.Maybe
root = "/var/bugpan/sessions/"


main = do
  allArgs <- getArgs
  let (opts, args) = partition beginsWithHyphen allArgs
  dispatch opts args

unCap [] = []
unCap (c:cs) = toLower c : cs

beginsWithHyphen ('-':_) = True
beginsWithHyphen _ = False

preprocessQuery qs | "@=" `isInfixOf` qs = let (lhs, rhs) = span (/='@') $ qs
                                           in "\""++(filter (/=' ') lhs)++"\" "++rhs
                   | otherwise = qs

withNothing x = (x,Nothing)                 

mkQuery :: [(String, T)] -> String -> String -> String -> [String]
mkQuery tps sessNm q resp = execWriter $ do
                          tell ["inSessionNamed "++sessNm++" $ do {"]
                          let ind = "          "
                          forM_ tps $ \(nm, ty) -> do
                                       if ty == SignalT (NumT (Just RealT))
                                          then tell $ [concat [ind,unCap nm ++ " <- signalsDirect ",
                                                       " \""++ nm++"\";" ]]
                                          else tell $ [concat [ind,unCap nm ++ " <- ",
                                                       typeToKind ty,
                                                       " \""++ nm++"\" ",
                                                       (typeToProxyName $ unWrapT ty), ";"
                                                      ]]
                          tell [ind++"qresval <- qResThroughSession ("++q++");"]
                          tell [ind++resp++"(qresval) }"]

dispatch opts ("testask":_) = do
  setCurrentDirectory "/var/bugpan/"
  out <- runInterpreter $ do
           setImportsQ $ map withNothing ["Prelude"]
           n <- interpret ("show $ 2+2") (as :: String)
           return n
  case out of
    Right outaction -> putStrLn outaction
    Left err -> print err
  return ()

dispatch opts ("testask1":sessNm:_) = do
  out<- inApproxSession sessNm $ do
          tStart <- events "tStart" ()
          tStop <- events "tStop" ()
          collision <- events "collision" ()
          tStart <- events "tStart" ()
          displacedAngle <- durations "displacedAngle" double
          program <- durations "program" "foo"
          moduleName <- durations "moduleName" "foo"
          displacedLoom <- durations "DisplacedLoom" ()
          qresval <- qResThroughSession (tStart)
          return $ QResBox (qresval)
  case out of
    QResBox qres -> do
             qreply <- qReply qres
             case safeLast $ lines qreply of
               Just s | "file://" `isPrefixOf` s -> 
                                      when ("-o" `elem` opts) $ do
                                        system $ "gnome-open "++s
                                        return ()
                      | otherwise -> return ()
               _ -> return ()
             putStrLn qreply
             return ()


dispatch opts ("ask":sessNm:queryStr':_) = do
  sess <- loadApproxSession root sessNm
  let sessNm = last . splitBy '/' $ baseDir sess 
  let queryStr = preprocessQuery queryStr'
  --putStrLn queryStr
  --print sessNm
  tps <- sessionTypes sess
  setCurrentDirectory "/var/bugpan/"
  --setResourceLimit ResourceOpenFiles $ ResourceLimits (ResourceLimit 32000) (ResourceLimit 32000) 
  --mapM_ print tps
  out <- runInterpreter $ do
           --loadModules ["Query", "QueryTypes", "QueryUtils"]
           let cmd = mkQuery tps ("\""++ sessNm++"\"") queryStr "return $ QResBox "
           --setTopLevelModules [""]
           setImportsQ $ map withNothing ["Prelude","Query", "QueryTypes", "QueryUtils", "Numbers", "Math.Probably.PlotR"]
           liftIO . putStrLn $ unlines cmd
           n <- interpret (unlines cmd) (as :: IO QueryResultBox)
           return n
  --QResBox qres <- (theQuery v) sessNm
  --print qres
  case out of
    Right outaction -> do QResBox qres <- outaction 
                          qreply <- qReply qres
                          case safeLast $ lines qreply of
                            Just s | "file://" `isPrefixOf` s -> 
                                                 when ("-o" `elem` opts) $ do
                                                      system $ "gnome-open "++s
                                                      return ()
                                   | otherwise -> return ()
                            _ -> return ()
                          putStrLn qreply
    Left err -> print err
  return ()

dispatch _ ("convert1":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  sigs <- getDirContents $ (oneTrailingSlash $ baseDir sess)++"signals"
  let path  = (oneTrailingSlash $ baseDir sess)++"signals/"
  forM_ sigs $ \sig -> do 
    vs<-loadUntyped1 $ path++sig
    createDirectory $ path++sig++"_conv"
    forM_ vs $ \(nm,sigv) -> do let fp = path++sig++"_conv/"++nm
                               --print fp
                                putStrLn $ "saving "++show sigv++ " in "++fp
                                saveBinary fp [sigv]
  --print sigs
      where loadUntyped1 :: FilePath -> IO [(String,V)]
            loadUntyped1 fp = do 
              ifM (doesDirectoryExist fp)
                  (do fnms <- getSortedDirContents fp
                      xs <- forM fnms $ \fn-> do b<-loadBinary $ fp++"/"++fn
                                                 return (fn,b)
                      return $ xs)
                  ((print $ "dir not found:" ++fp) >> return [])

dispatch _ ("convert2":sessNm:_) = do
  sessOld <- loadApproxSession root sessNm
  sessNew <- cloneSession sessOld "_fmt3" 3
  forM_ ["signals", "events", "durations"] $ \kind -> do
                                   sigs <- getDirContents $ (oneTrailingSlash $ baseDir sessOld)++kind
                                   let path  = (oneTrailingSlash $ baseDir sessOld)++kind++"/"
                                   let pathN  = (oneTrailingSlash $ baseDir sessNew)++kind++"/"
                                   --putStrLn $ "kind="++kind
                                   forM_ sigs $ \sig -> do 
                                                       --putStrLn $ "sig="++sig
                                                       vs<-loadUntyped1 $ path++sig
                                                       createDirectory $ pathN++sig
                                                       forM_ vs $ \(nm,sigv) -> do let fp = pathN++sig++"/"++nm
                                                                                   putStrLn $ "saving "++show sigv++ " in "++fp
                                                                                   saveVs fp sigv
      where loadUntyped1 :: FilePath -> IO [(String,[V])]
            loadUntyped1 fp = do 
              ifM (doesDirectoryExist fp)
                  (do fnms <- getSortedDirContents fp
                      xs <- forM fnms $ \fn-> do b <- loadBinary $ fp++"/"++fn
                                                 --return $ idLstOldFmtV b
                                                 --putStrLn $ fn ++ show b
                                                 return (fn,map oldVtoV b)
                      return $ xs)
                  ((print $ "dir not found:" ++fp) >> return [])

dispatch _ ("compact":sessNm:_) = do
  system $ "./BugSess compact_1 "++sessNm
  system $ "./BugSess compact_2 "++sessNm
  return ()

dispatch _ ("compact_1":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         fnms <- getSortedDirContents $ path++nm
         xs <- forM fnms $ \fn->loadBinary $ path++nm++"/"++fn                                    
         let vs = sortVs $ idLstV $ concat xs
         saveBinary (path++nm++"/compacted") vs
         --putStrLn $ nm ++ ": "++ppVal (ListV vs)

dispatch _ ("compact_2":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         fnms <- getSortedDirContents $ path++nm
         forM fnms $ \fn-> if fn == "compacted"
                              then return () --print "skipping"
                              else removeFile $ path++nm++"/"++fn
                    
dispatch _ ("list":_) = do
  sesns <- getSessionInRootDir root
  forM_ sesns $ \sNm -> do
      putStr $ sNm++": "
      inSessionNamed sNm $ do 
                          --prg <- durations "program" ""
                          modNm <- durations "moduleName" ""
                          liftIO . putStr $ showDiffModules $ map snd modNm
      putStrLn ""
 
dispatch _ ("filter":filtr:_) = do
  sesns <- getSessionInRootDir root
  setCurrentDirectory "/var/bugpan/"

  lallNmTps <- forM sesns $ \sNm -> do
                                  sess <- loadExactSession $ root++sNm
                                  sessionTypes sess
  let allNmTps = concat lallNmTps
  let allNms = nub $ map fst $ allNmTps
  let nmtys = catMaybes $ for allNms $ \nm->case nub $ lookupMany nm allNmTps of
                                              (ty:[]) -> Just (nm,ty)
                                              _ -> Nothing --excludes ambiguous
  let spliceFirst spl (s:ss) = (spl++s):ss
  let filtFunStr = spliceFirst "\\sess -> " $ mkQuery nmtys ("sess") filtr "return $ qFilterSuccess "
  --putStrLn $ unlines filtFunStr  
  filtrFunO<- runInterpreter $ do
                --loadModules ["Query", "QueryTypes", "QueryUtils"]
                setImportsQ $ map withNothing ["Prelude","Query", "QueryTypes", 
                                               "QueryUtils", "Numbers", "Math.Probably.PlotR"]          
                n <- interpret (unlines filtFunStr) (as :: String -> IO Bool)
                return n
  --QResBox qres <- (theQuery v) sessNm
  --print qres
  filFun <- case filtrFunO of
              Right fitlrFun -> do return fitlrFun
              Left err -> fail $ show err
  forM_ sesns $ \sNm -> do
    pass <- filFun sNm
    putStrLn $ sNm++": "++show pass
 



dispatch opts ("show":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  tps <- sessionTypes sess
  when (not $ "-t" `elem` opts) $ do
    (t1,t2) <- read `fmap` readFile (baseDir sess++"/tStart")
    let t0 = TOD t1 t2
    putStrLn $ "Start time: "++ show t0
    inSession sess $ do 
       --prg <- durations "program" ""
      modNm <- durations "moduleName" ""
      liftIO . putStrLn $ "Modules run: "++(showDiffModules $ map snd modNm)
  putStrLn "Values:"
  forM_ tps $ \(nm, ty) -> do
    putStrLn $ "\t"++(unCap nm)++" :: "++pTy ty
        where pTy (PairT (PairT (NumT (Just RealT)) (NumT (Just RealT))) t) = "Duration "++ppType t
              pTy (PairT (NumT (Just RealT)) t) = "Event "++ppType t
              pTy t = ppType t

dispatch _ _ = putStrLn $ unlines [
              "",
              "Manage bugpan sessions",
              "",
              "\tBugSess ask {session} {query}",
              "\tBugSess show {session}",
              "\tBugSess list",
              "\tBugSess filter {query}",
              "\tBugSess compact {session}",
              "\tBugSess convert2 {session}",
              "\tBugSess convert1 {session}"

 ]

-- find . -name "compacted" -exec rm -rf {} \;

showDiffModules :: [String] -> String
showDiffModules mods = intercalate ", " . map f $ group mods
    where f mods =(unCap $head mods) ++ "("++ (show $ length mods)++")"

idLstV :: [V] -> [V]
idLstV = id


-- :set -fbreak-on-exception

-- :trace dispatch ["convert2", "72"]
typeToKind :: T -> String
typeToKind (SignalT _) = "signals"
typeToKind (PairT (PairT _ _) _) = "durations"
typeToKind (PairT (NumT _) _) = "events"

unWrapT :: T-> T
unWrapT (SignalT t) = t
unWrapT (PairT (PairT _ _) t) = t
unWrapT (PairT (NumT _) t) = t
unWrapT t = t


typeToProxyName :: T-> String
typeToProxyName UnitT = "()"
typeToProxyName StringT = "\"foo\""
typeToProxyName (NumT (Just RealT)) = "double"
typeToProxyName (PairT t1 t2) = "("++typeToProxyName t1++", "++typeToProxyName t2++")"

