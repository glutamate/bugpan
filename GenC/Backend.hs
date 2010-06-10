module Main where

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
--import CompiledSrcsSinks
import Data.Maybe 
import TNUtils
import EvalM
import Query (bugpanRootDir) 
import Syntax
import Parse
import System.Environment
import Numbers
--forget about sinks, sources apart fro

{-

ghc --make Backend.hs && ./Backend Test.bug 

&& gcc Test.c -lm -o Test

-}

compileToC fp dt tmax ds params = do  
  --mapM print ds

  --putStrLn "\n---------------\n"
  let (env:stageDs) = splitByStages ds
  putStrLn "\n---------------s0\n"
  mapM print $ head stageDs
  let prg = ppCProg $ toC dt tmax ds params
  writeFile (fp) prg
  --putStrLn prg 
  return ()

gloDbl nm x=DeclareGlobal CDoubleT nm (Just (Const (NumV $ NReal x))) 

toC dt tmax ds params
    = let (env:stageDs) = splitByStages ds
          stages = zip [0..] stageDs
      in concat [imports, 
                 [gloDbl "dt" dt, gloDbl "tmax" tmax], 
                 globals ds, 
                 concatMap stepper stages, 
                 [mainFun ds stages]]

mainFun ds stages
    = CFun CIntT "main" [("argc", CIntT), 
                             ("argv", CPtrT $ CPtrT CCharT)] $
               [Assign (Var "npnts") (Var "tmax"/Var"dt")]++         
               concat(nub $ map (mainBeg ds) ds)++
               concatMap runStage stages++
               concatMap (mainEnd ds) ds++
               [Return 1]

runStage (n, ds) = concatMap runOnceSrcs ds++ driveStage (n, ds) 

driveStage (n, ds) = 
    [forCount "i" 0 (Var "npnts") [Call ("step"++show n) []]]

runOnceSrcs (ReadSource vnm ("poisson", rate)) = [Assign (Var vnm) (Var "poisson_train" $> rate $> Var "tmax")]
runOnceSrcs _ = []


imports = map (CInclude True) ["stdlib.h","stdio.h", "math.h"] ++ [CInclude False "dynprelude.c"]

mainBeg ds (SinkConnect (Var nm) ("store", _)) = 
        let t =tyOf ds nm in
        [Assign (Var $ nm++"Sig")
                (Var "create_sig" $> Var "npnts")]
                --(Var "malloc" $> (Var "npnts" * (Var "sizeof" $> Var (ppCTy $ bugTyToCTy t)))), 
mainBeg ds (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var $ nm++"Sig")
                (Var "create_sig" $> Var "npnts")]
                                       | otherwise = []

         
mainBeg _ _ = []

mainEnd ds (SinkConnect (Var nm) ("store", _))= 
    [Call "write_signal" [Const (StringV nm), Var (nm++"Sig")],
     Call "free_sig" [Var (nm++"Sig")]]
mainEnd ds _ = []

globals ds = concat(nub $ map (gloVar ds) ds)++
             [DeclareGlobal CLongT "npnts" Nothing, 
              DeclareGlobal CLongT "i" (Just 0)]

gloVar _ (Let (PatVar nm t) (Sig e)) = 
          [DeclareGlobal (bugTyToCTy t) nm Nothing]
gloVar _ (Let (PatVar nm t) (Const v)) = 
          [DeclareGlobal (bugTyToCTy t) nm (Just $Const v)]
gloVar _ (Let (PatVar nm ft) lam@(Lam _ _ _)) = 
          let nms = flatLamNms lam
              tys = map bugTyToCTy $ flatLamTy ft
          in [CFun (last tys) nm (zip nms $ init tys) [Return $ lamBody lam]]
gloVar _ (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [DeclareGlobal (bugTyToCTy t) (nm) (Just v)]
gloVar ds (SinkConnect (Var nm) ("store", _)) = 
          let t =tyOf ds nm in
          [DeclareGlobal (CPtrT $ CStructT "signal_double") (nm++"Sig") Nothing]
gloVar ds (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
          let t =tyOf ds nm in
          [DeclareGlobal (CPtrT $ CStructT "signal_double") (nm++"Sig") Nothing]
                                            | otherwise = []
gloVar ds (ReadSource vnm ("poisson", rate)) = 
          let t =tyOf ds vnm in
          [DeclareGlobal (bugTyToCTy t) (vnm) Nothing]
gloVar _ _ = []

flatLamTy (LamT arg res) = flatLamTy arg ++ flatLamTy res
flatLamTy t = [t]

flatLamNms (Lam nm _ e) = nm:flatLamNms e
flatLamNms e = []       

lamBody (Lam nm _ e) = lamBody e
lamBody e = e

tyOf ds nm =  head $ [t | DeclareType nm' t <- ds, nm == nm' ] 

stepper (stage,ds) = [CFun CIntT ("step"++show stage) [] $ secs:(concat$ nub $map step ds)]
    where secs = DecVar CDoubleT "seconds" (Just $ Var "i"*Var "dt")
step (Let (PatVar nm t) (Sig e)) = [Assign (Var (nm)) $ unVal e]
step (SinkConnect (Var nm) ("store", _)) = [Assign (Var (nm++"Sig->arr[i]")) $ Var nm]
step (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [Assign (Var (nm)) $ (Var nm) + Var "dt" * unVal e]
step (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var (nm++"Sig->arr[i]")) $ Var nm]
                                       | otherwise = []

step d = []

main = do
   file :_ <- getArgs
   ds' <- fileDecls file []
   let ds =let runTM = runTravM ds' [] in snd . runTM $ transform
   compileToC ((head $ splitBy '.' file)++".c") (getDt ds) (getTmax ds) ds []


getTmax ds
    = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1

getDt ds
    =  (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001

unVal = mapE u
  where u (SigVal (Var nm)) = Var $ nm 
        u e = e

--stored signals
--parenthesis in pretty printer
--lets in functions