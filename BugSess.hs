module Main where

import Database
import Query
import System.Environment
import TNUtils
import EvalM
import System.Directory

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
import System.Exit
import Data.Maybe
import System.Process
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal
import PlotGnuplot
import QueryUtils
import QueryPlots

root = "/var/bugpan/sessions/"


main = do
  allArgs <- getArgs
  let (opts, args) = partition beginsWithHyphen allArgs
  dispatch opts args

preprocessQuery qs | "@=" `isInfixOf` qs = let (lhs, rhs) = span (/='@') $ qs
                                           in "\""++(filter (/=' ') lhs)++"\" "++rhs
                   | otherwise = qs

--matched parens, never negative level
--escape quotes?
validateQuery q = checkParens q 0

checkParens :: String -> Int -> Bool
checkParens [] 0 = True
checkParens [] _ = False
checkParens ('(':s) n = checkParens s (n+1)
checkParens (')':s) n | n < 1 = False
                      | otherwise = checkParens s (n-1)
checkParens (_:s) n = checkParens s n

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

compileQuery opts sha q = do
  --putStrLn $ "compiling query "++q
  sesns <- getSessionInRootDir root
  nmtys <- getNamesAndTypes sesns
  whenM (not `fmap` doesDirectoryExist "/var/bugpan/queryCache/") 
        (createDirectory "/var/bugpan/queryCache/") 
  setCurrentDirectory "/var/bugpan/queryCache/"
  let initModule = unlines $ "module Main where":(map ("import "++) ["Prelude","Query", "QueryTypes", "QueryPlots", 
                                                                    "QueryUtils", "Numbers",
                                                                    "System.Environment","Data.List", "TNUtils",
                                                                    "PlotGnuplot"])
                   ++["default (Int, Double)"]
      
  let mainFun = ["querySha = \""++sha++"\"", 
                 "",
                 "main = do",
                 "  allArgs <- getArgs",
                 "  let (opts, sess:_) = partition beginsWithHyphen allArgs",
                 "  qres <- q sess",
                 "  let resDir = take 10 querySha ++ take 10 sess",
                 "  if not $ \"-filter\" `elem` opts",
                 "     then (do qreply <- qReply qres $ (\"-d\"++resDir):opts",
                 "              putStrLn qreply)",
                 "     else (do pass <- return $ qFilterSuccess qres",
                 "              putStrLn $ sess++\": \"++show pass)"]
  let qFun = spliceFirst "q sess = " $ mkQuery nmtys ("sess") q "return "
  let allmod = initModule ++ unlines qFun ++ unlines mainFun
  --putStrLn allmod
  writeFile (sha++".hs") $ allmod
  let ghcout = " >ghcout`whoami` 2>ghcout2`whoami`"
  --let ghcout = " >ghcout 2>ghcout2"

  sysres <- if ("-p" `elem` opts)
               then system $ "ghc --make -prof -auto-all "++sha ++ghcout
               else system $ "ghc --make -O2 "++sha ++ghcout
  case sysres of 
    ExitSuccess -> return ()
    ExitFailure n -> do putStrLn allmod
                        system $ "cat ghcout`whoami`"
                        system $ "cat ghcout2`whoami`"
                        fail $ "compile query fails ("++show n++")"

getNamesAndTypes sesns = do 
  lallNmTps <- forM sesns $ \sNm -> do
                                  sess <- loadExactSession $ root++sNm
                                  sessionTypes sess
  let allNmTps = concat lallNmTps
  let allNms = nub $ map fst $ allNmTps
  let nmtys = catMaybes $ for allNms $ \nm->case nub $ lookupMany nm allNmTps of
                                              (ty:[]) -> Just (nm,ty)
                                              _ -> Nothing --excludes ambiguous
  return nmtys


dispatch opts ("ask1":sessNm:queryStr':_) = do
  queryStr <- if "-f" `elem` opts 
                 then preprocessQuery `fmap` readFile queryStr'
                 else return $ preprocessQuery queryStr'
  when (not $ validateQuery queryStr) $
       fail $ "cannot validate query string"
  let sha = take 50 . showDigest . sha512 . BS.pack $ map c2w queryStr
  --putStrLn sha
  whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
        (compileQuery opts sha queryStr)
  longSessNm <- resolveApproxSession root sessNm
  if ("-p" `elem` opts)
     then system $ "/var/bugpan/queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " opts)++" +RTS -p"
     else system $ "/var/bugpan/queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " opts)
  

  return ()


dispatch opts ("ask":sessNm:queryStr':_) = do
  sess <- loadApproxSession root sessNm
  let sessNm = last . splitBy '/' $ baseDir sess 
  let queryStr = preprocessQuery queryStr'
  when (not $ validateQuery queryStr) $
       fail $ "cannot validate query string"
  --putStrLn queryStr
  --print sessNm
  tps <- sessionTypes sess
  setCurrentDirectory "/var/bugpan/"
  --setResourceLimit ResourceOpenFiles $ ResourceLimits (ResourceLimit 32000) (ResourceLimit 32000) 
  --mapM_ print tps
  let cmd = mkQuery tps ("\""++ sessNm++"\"") queryStr "return $ QResBox "
  out <- runInterpreter $ do
           --loadModules ["Query", "QueryTypes", "QueryUtils"]
           --setTopLevelModules [""]
           setImportsQ $ map withNothing ["Prelude","Query", "QueryTypes", "QueryUtils", 
                                          "Numbers", "Math.Probably.PlotR"]
           
           n <- interpret (unlines cmd) (as :: IO QueryResultBox)
           return n
  --QResBox qres <- (theQuery v) sessNm
  --print qres
  case out of
    Right outaction -> do 
             QResBox qres <- outaction 
             qreply <- qReply qres opts
             case dropPrefix "file:///var/bugpan/www/" qreply of
               ("",s) -> cond [("-o" `elem` opts,  do
                                        system $ "gnome-open "++qreply
                                        return ()),
                              ("-s" `elem` opts, do 
                                 system "ifconfig eth0 | perl -n -e 'if (m/inet addr:([\\d\\.]+)/g) { print $1 }' | cat >/tmp/my_ip_address" 
                                 ip <- readFile "/tmp/my_ip_address"
                                 putStrLn $ "http://"++ip++"/"++s)] $ putStrLn qreply
               _ -> putStrLn qreply

                     
    Left err -> do putStrLn $ unlines cmd 
                   print err
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
  system $ "bugsess compact_1 "++sessNm
  system $ "bugsess compact_2 "++sessNm
  return ()

dispatch _ ("compact_1":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         allfnms <- getSortedDirContents $ path++nm
         let fnms = filter (not . ("compacted" `isPrefixOf`)) allfnms
--         print2 ("files to compact in "++nm) fnms
         
         let fileNoMax = foldl (max) 0 $ catMaybes $ map (safeRead . (drop 9)) $ filter ("compacted" `isPrefixOf`) allfnms
--         print2 "filemaxno " fileNoMax
         xs <- forM fnms $ \fn->loadBinaryStrict $ path++nm++"/"++fn                                    
         let vs = sortVs $ idLstV $ concat xs
         saveBinary (path++nm++"/compacted"++show (fileNoMax + idInt 1)) vs
         --putStrLn $ nm ++ ": "++ppVal (ListV vs)

dispatch _ ("compact_2":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         fnms <- getSortedDirContents $ path++nm
         forM fnms $ \fn-> if ("compacted" `isPrefixOf` fn)
                              then return () --print "skipping"
                              else removeFile $ path++nm++"/"++fn
                    
dispatch opts ("list":_) = do
  sesns <- getSessionInRootDir root
  forM_ sesns $ \sNm -> do
      putStr $ sNm
      inSessionNamed sNm $ do 
                          --prg <- durations "program" ""
                          Session bdir _ <- getSession
                          let vfile = (oneTrailingSlash bdir ++ "sessionFormatVersion")
                          ifM (liftIO $ doesFileExist vfile)
                              ( do v <- liftIO $ readFile vfile
                                   liftIO. putStr $ " (v"++v++")")
                              (return ())
                          when ("-m" `elem` opts) $ do
                             modNm <- durations "moduleName" ""
                             liftIO . putStr $ ": "++(showDiffModules $ map snd modNm)
      putStrLn ""

dispatch _ ("filter":filtr:_) = do
  sesns <- getSessionInRootDir root
  setCurrentDirectory "/var/bugpan/"
  nmtys <- getNamesAndTypes sesns

  let filtFunStr = spliceFirst "\\sess -> " $ mkQuery nmtys ("sess") filtr "return $ qFilterSuccess "
  when (not $ validateQuery filtr) $
       fail $ "cannot validate query string"

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
 
dispatch opts ("filter1":queryStr':_) = do
  sesns <- getSessionInRootDir root
  setCurrentDirectory "/var/bugpan/"
  queryStr <- if "-f" `elem` opts 
                 then preprocessQuery `fmap` readFile queryStr'
                 else return $ preprocessQuery queryStr'
  when (not $ validateQuery queryStr) $
       fail $ "cannot validate filter string"
  let sha = take 50 . showDigest . sha512 . BS.pack $ map c2w queryStr
  --putStrLn sha
  whenM (not `fmap` doesFileExist ("/var/bugpan/queryCache/"++sha)) 
        (compileQuery opts sha queryStr)
  --longSessNm <- resolveApproxSession root sessNm
  forM_ sesns $ \sNm -> do
    longSessNm <- resolveApproxSession root sNm
    system $ "/var/bugpan/queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " (("-filter"):opts))
  

  return ()


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

dispatch opts ("plotsigs":sessNm:sigNm:_) = do
  qres <- inApproxSession sessNm $ do
            sig <- signalsDirect sigNm
            return $ plotManySigs sig
  qreply <- qReply qres opts
  putStrLn qreply

  return ()

dispatch opts ("mksdur":sessNm:durnm:val:_) = do
  qres <- inApproxSession sessNm $ do
            storeAsOvwrt durnm $ dur val
  return ()


dispatch opts ("mkndur":sessNm:durnm:val:_) = do
  qres <- inApproxSession sessNm $ do
            case safeRead val of
              Just x -> storeAsOvwrt durnm (durd x) >> return ()
              Nothing -> return ()
  return ()

    
dispatch _ _ = putStrLn $ unlines [
              "",
              "Manage bugpan sessions",
              "",
              "\tbugsess ask {session} {query}",
              "\tbugsess show {session}",
              "\tbugsess list",
              "\tbugsess filter {query}",
              "\tbugsess compact {session}",
              "\tbugsess convert2 {session}",
              "\tbugsess convert1 {session}"

 ]

-- find . -name "compacted" -exec rm -rf {} \;

showDiffModules :: [String] -> String
showDiffModules mods = intercalate ", " . map f $ group mods
    where f mods =(unCap $head mods) ++ "("++ (show $ length mods)++")"

idLstV :: [V] -> [V]
idLstV = id


myfun ("foo") = True




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
typeToProxyName (NumT (Just IntT)) = "(1::Int)"
typeToProxyName (NumT (Just RealT)) = "double"
typeToProxyName (PairT t1 t2) = "("++typeToProxyName t1++", "++typeToProxyName t2++")"
typeToProxyName BoolT = "True"
typeToProxyName t = error $ "typeToProxyName: unknown type "++show t

