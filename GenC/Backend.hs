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

compileToC fp dt tmax ds params = do  
  mapM print ds

  putStrLn "\n---------------\n"

  let prg = ppCProg $ toC dt tmax ds params
  writeFile (fp) prg
  putStrLn prg 
  return ()

gloDbl nm x=DeclareGlobal CDoubleT nm (Just (Const (NumV $ NReal x))) 

toC dt tmax ds params
    = concat [imports, [gloDbl "dt" dt, gloDbl "tmax" tmax], globals ds, stepper ds, [mainFun ds]]

mainFun ds = CFun CIntT "main" [("argc", CIntT), 
                             ("argv", CPtrT $ CPtrT CCharT)] $
               [Call "printf" [Const (StringV "hello world\n")]]
               ++concatMap (mainBeg ds) ds++
               [forCount "i" 0 (Var "npnts") [Call "step" []]]++
               concatMap (mainEnd ds) ds++
               [Return 1]


imports = map CInclude ["stdlib.h","stdio.h", "math.h"]

mainBeg ds (SinkConnect (Var nm) ("store", _)) = 
        let t =tyOf ds nm in
        [Assign (Var $ nm++"Sig")
                (Var "malloc" $> (Var "npnts" * (Var "sizeof" $> Var (ppCTy $ bugTyToCTy t)))) ]
mainBeg _ _ = []

mainEnd ds (SinkConnect (Var nm) ("store", _))= [Call "free" [Var (nm++"Sig")]]
mainEnd ds _ = []

globals ds = concatMap (gloVar ds) ds++
             [DeclareGlobal CIntT "npnts" (Just $ Var "tmax"/Var"dt"), 
              DeclareGlobal CIntT "i" (Just 0)]

gloVar _ (Let (PatVar nm t) (Sig e)) = 
          [DeclareGlobal (bugTyToCTy t) nm Nothing]
gloVar _ (Let (PatVar nm ft) lam@(Lam _ _ _)) = 
          let nms = flatLamNms lam
              tys = map bugTyToCTy $ flatLamTy ft
          in [CFun (last tys) nm (zip nms $ init tys) [Return $ lamBody lam]]
gloVar _ (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [DeclareGlobal (bugTyToCTy t) (nm) (Just v)]
gloVar ds (SinkConnect (Var nm) ("store", _)) = 
          let t =tyOf ds nm in
          [DeclareGlobal (CPtrT $ bugTyToCTy t) (nm++"Sig") Nothing]
gloVar _ _ = []

flatLamTy (LamT arg res) = flatLamTy arg ++ flatLamTy res
flatLamTy t = [t]

flatLamNms (Lam nm _ e) = nm:flatLamNms e
flatLamNms e = []       

lamBody (Lam nm _ e) = lamBody e
lamBody e = e

tyOf ds nm =  head $ [t | DeclareType nm' t <- ds, nm == nm' ] 

stepper ds = [CFun CIntT "step" [] $ concatMap step ds]
step (Let (PatVar nm t) (Sig e)) = [Assign (Var (nm)) $ unVal e]
step (SinkConnect (Var nm) ("store", _)) = [Assign (Var (nm++"Sig[i]")) $ Var nm]
step (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [Assign (Var (nm)) $ (Var nm) + Var "dt" * unVal e]

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