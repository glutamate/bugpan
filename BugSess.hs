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

root = "/home/tomn/sessions/"


main = do
  args <- getArgs
  dispatch args

dispatch ("ask":sessNm:queryStr:_) = do
  sess <- loadApproxSession root sessNm
  let sessNm = last . splitBy '/' $ baseDir sess 
  --print sessNm
  tps <- sessionTypes sess
  --mapM_ print tps
  out <- runInterpreter $ do
           loadModules ["Query", "QueryTypes", "QueryUtils"]
           cmd <- execWriterT $ do
                          tell ["inSessionNamed \""++sessNm++"\" $ do"]
                          let ind = "          "
                          forM_ tps $ \(nm, ty) -> do
                                       tell $ [concat [ind,nm ++ " <- ",
                                                       typeToKind ty,
                                                       " \""++ nm++"\" ",
                                                       (typeToProxyName $ unWrapT ty)
                                                      ]]
                          tell [ind++"return $ QResBox ("++queryStr++")"]
           
           --setTopLevelModules [""]
           setImportsQ $ map withNothing ["Prelude","Query", "QueryTypes", "QueryUtils"]
           liftIO . putStrLn $ unlines cmd
           n <- interpret (unlines cmd) (as :: IO QueryResultBox)
           return n
  --QResBox qres <- (theQuery v) sessNm
  --print qres
  case out of
    Right outaction -> outaction >>= (\(QResBox qres) -> print qres)
    Left err -> print err
  return ()
      where withNothing x = (x,Nothing) 

dispatch ("convert1":sessNm:_) = do
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

