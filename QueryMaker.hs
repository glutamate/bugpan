module QueryMaker where

import Control.Monad.Writer.Lazy
import Database
import Query
import System.Environment
import TNUtils (unCap, spliceFirst, for, whenM, lookupMany)
import EvalM
import Data.List
import Data.Maybe
import System.Cmd
import System.Directory
import System.Exit


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
                          tell [ind++"sessionName <- dur `fmap` getSessionName;"]
                          tell [ind++"qresval <- qResThroughSession ("++q++");"]
                          tell [ind++resp++"(qresval) }"] 

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

root = "/var/bugpan/sessions/"


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
                                                                    "PlotGnuplot", "NewSignal"])
                   ++["default (Int, Double)"]
      
  let mainFun = ["querySha = \""++sha++"\"", 
                 "",
                 "main = do",
                 "  allArgs <- getArgs",
                 "  let (opts, sess:restargs) = partition beginsWithHyphen allArgs",
                 "  qres <- q sess",
                 "  let resDir = take 10 querySha ++ take 10 sess",
                 "  let newargs = (sess:restargs)++((\"-d\"++resDir):opts)",
                 "  if not $ \"-filter\" `elem` opts",
                 "     then (do qreply <- withArgs newargs $ qReply qres $ (\"-d\"++resDir):opts",
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

