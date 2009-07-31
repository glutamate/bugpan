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

--forget about sinks, sources apart from store


--hide sum, map, snd, fst.
--translate, box?

--compileToHask :: String -> [Declare] -> IO String
compileToHask modNm dt tmax ds = do
  let prg = toHask modNm dt tmax ds
  writeFile (capitalize modNm++".hs") prg
  putStrLn prg
  return prg

--toHask :: String -> [Declare] -> String
toHask modNm dt tmax ds 
    = let (env:stageDs) = splitByStages ds
          nonMainLoop = filter notbanned $ fst (runTravM env [] compilableEnv) 
      in unlines $ ["{-# LANGUAGE NoMonomorphismRestriction #-}",
                    "module "++capitalize modNm++" where",
                    "",
                    "import Numbers", 
                    "import HaskShapes","",
                   "dt="++show dt, "tmax="++show tmax]++
                   map atTopLevel nonMainLoop

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