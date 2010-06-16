module Backend where

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
  let stgs@(env:stageDs) = splitByStages ds
  forM stgs $ \ds-> do
    putStrLn "\n---------------stage\n"
    mapM print  ds
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
               [Assign (Var "npnts") $ Var "llround" $> (Var "tmax"/Var"dt")]++  
               [traceInt "npnts" "npnts"]++
               concat(nub $ map (mainBeg ds) ds)++
               concatMap runStage stages++
               concatMap (mainEnd ds) ds++
               [Return 1]

runStage (n, ds) = [traceS $"stage"++show n ]++concatMap runOnceSrcs ds++ driveStage (n, ds) 

isDynClamp ds = 
    let ins = [() | ReadSource _ ("ADC",_) <- ds ]
        outs = [() | SinkConnect _ ("DAC",_) <- ds ]
    in not (null ins || null outs)

driveStage (n, ds) 
    | isDynClamp ds = [Call ("stepdyn"++show n) []]
    | otherwise = [forCount "i" 0 (Var "npnts") [Call ("step"++show n) []]]

runOnceSrcs (ReadSource vnm ("poisson", rate)) = [Assign (Var vnm) (Var "poisson_train" $> rate $> Var "tmax")]
runOnceSrcs _ = []

imports = map (CInclude True) ["stdlib.h","stdio.h", "math.h"] ++ [CInclude False "dynprelude.c"]

mainBeg ds (SinkConnect (Var nm) ("store", _)) = 
        let t =tyOf ds nm in
        [Assign (Var $ nm)
                (Var "create_sig" $> Var "npnts")]
                --(Var "malloc" $> (Var "npnts" * (Var "sizeof" $> Var (ppCTy $ bugTyToCTy t)))), 
mainBeg ds (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var $ nm)
                (Var "create_sig" $> Var "npnts")]
                                       | otherwise = []

         
mainBeg _ _ = []

mainEnd ds (SinkConnect (Var nm) ("store", _))= 
    [Call "write_signal" [Const (StringV ("dyn_sig_"++nm)), Var (nm)],
     Call "free_sig" [Var (nm)]]
mainEnd ds _ = []

globals ds = concat(nub $ map (gloVar ds) ds)++
             [DeclareGlobal CLongT "npnts" Nothing, 
              DeclareGlobal CLongT "i" (Just 0)]

gloVar _ (Let (PatVar nm t) (Sig e)) = 
          [DeclareGlobal (bugTyToCTy t) (nm++"Val") Nothing]
gloVar _ (Let (PatVar nm t) (Forget tm e)) = 
          [DeclareGlobal (bugTyToCTy t) nm Nothing]
gloVar _ (Let (PatVar nm t) (Const v)) = 
          [DeclareGlobal (bugTyToCTy t) nm (Just $Const v)]
gloVar _ (Let (PatVar nm ft) lam@(Lam _ _ _)) = 
          let nms = flatLamNms lam
              tys = map bugTyToCTy $ flatLamTy ft
          in [CFun (last tys) nm (zip nms $ init tys) [Return $ lamBody lam]]
gloVar _ (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [DeclareGlobal (bugTyToCTy t) (nm++"Val") (Just v)]
gloVar ds (SinkConnect (Var nm) ("store", _)) = 
          let t =tyOf ds nm in
          [DeclareGlobal (CPtrT $ CStructT "signal_double") (nm) Nothing]
gloVar ds (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
          let t =tyOf ds nm in
          [DeclareGlobal (CPtrT $ CStructT "signal_double") (nm) Nothing]
                                            | otherwise = []
gloVar ds (ReadSource vnm ("poisson", rate)) = 
          let t =tyOf ds vnm in
          [DeclareGlobal (bugTyToCTy t) (vnm) Nothing]
gloVar ds (ReadSource vnm ("ADC", rate)) = 
          [DeclareGlobal (CDoubleT) (vnm++"Val") Nothing]
gloVar _ _ = []

flatLamTy (LamT arg res) = flatLamTy arg ++ flatLamTy res
flatLamTy t = [t]

flatLamNms (Lam nm _ e) = nm:flatLamNms e
flatLamNms e = []       

lamBody (Lam nm _ e) = lamBody e
lamBody e = e

tyOf ds nm =  head $ [t | DeclareType nm' t <- ds, nm == nm' ] 

stepper (stage,ds) = [CFun CIntT ("step"++show stage) [] $ (secs:(concat$ nub $map step ds))++tr]
                     ++dynStepper (stage,ds)
    where secs = DecVar CDoubleT "secondsVal" (Just $ Var "i"*Var "dt")
          tr = [] -- [traceD ("step"++show stage) "secondsVal" ]
step (Let (PatVar nm t) (Sig e)) = [Assign (Var (nm++"Val")) $ unVal e]
step (Let (PatVar nm t) (Forget tm e)) = 
          [Assign (Var (nm)) (Var "forget_events" $> e $> (Var "secondsVal"- tm))]
step (SinkConnect (Var nm) ("store", _)) = [Assign (Var ("("++nm++"->arr)[i]")) $ Var  (nm++"Val")]
step (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [Assign (Var (nm++"Val")) $ (Var (nm++"Val")) + Var "dt" * unVal (SigVal e)]
step (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var ("("++nm++"->arr)[i]")) $ Var (nm++"Val")]
                                       | otherwise = []
step d = []

dynStepper (stage,ds) 
    | isDynClamp ds = [CFun CIntT ("stepdyn"++show stage) [] $ dynBegin++dynLoop ds++dynEnd]
    | otherwise = []

dynBegin = 
    [LitCmds 
       ["RTIME until;",
	"RT_TASK *task;",
	"comedi_insn insn_read;",
	"comedi_insn insn_write;",
	"lsampl_t sinewave;",
	"double actualtime;",
	"lsampl_t *hist;",
	"lsampl_t data[NCHAN];",
	"long i, k, n, retval;",
	"signal(SIGKILL, endme);",
	"signal(SIGTERM, endme);",
	"hist = malloc(SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);",
	"memset(hist, 0, SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);",

	"start_rt_timer(0);",
	"task = rt_task_init_schmod(nam2num(\"MYTASK\"), 1, 0, 0, SCHED_FIFO, 0xF);",
	
	"mlockall(MCL_CURRENT | MCL_FUTURE);",
	"rt_make_hard_real_time();", 
	"if (init_board()) {;",
	"       printf(\"Board initialization failed.\\n\");",
	"       return 1;",
	"}",
	"BUILD_AREAD_INSN(insn_read, subdevai, data[0], 1, read_chan[i], AI_RANGE, AREF_GROUND);",
	"BUILD_AWRITE_INSN(insn_write, subdevao, data[NICHAN + i], 1, write_chan[i], AO_RANGE, AREF_GROUND);",
        "until = rt_get_time();"]]

dynLoop ds =
    [forCount "i" 0 (Var "npnts") $ [
                 DecVar CDoubleT "secondsVal" (Just $ Var "i"*Var "dt"),
                 Call "comedi_do_insn" [Var "dev", Var "insn_read"]]++
                 (concat$ nub $map stepd ds)]
	
dynEnd = 
    [LitCmds 
       ["comedi_cancel(dev, subdevai);",
	"comedi_cancel(dev, subdevao);",
	"comedi_data_write(dev, subdevao, 0, 0, AREF_GROUND, 2048);",
	"comedi_data_write(dev, subdevao, 1, 0, AREF_GROUND, 2048);",
	"comedi_close(dev);"],
    LitCmds 
       ["free(hist);",
	"stop_rt_timer();",
	"rt_make_soft_real_time();",
	"rt_task_delete(task);"]]

stepd (SinkConnect (Var nm) ("DAC", _)) = [Assign  (Var "data[1]") (Var (nm++"Val")) ]
stepd (ReadSource nm ("ADC", _)) = [Assign (Var (nm++"Val")) $ Var "data[0]"]
stepd d = step d

traceD what nm = Call "printf" [Const (StringV $ what ++ " " ++ nm ++" %g\n"), Var nm]
traceInt what nm = Call "printf" [Const (StringV $ what ++ " " ++ nm ++" %ld\n"), Var nm]
traceS what= Call "printf" [Const (StringV $ what++"\n")]


{-main = do
   file :_ <- getArgs
   ds' <- fileDecls file []
   let ds =let runTM = runTravM ds' [] in snd . runTM $ transform
   compileToC ((head $ splitBy '.' file)++".c") (getDt ds) (getTmax ds) ds [] -}


getTmax ds
    = (lookupDefn "_tmax" ds >>= vToDbl) `orJust` 1

getDt ds
    =  (lookupDefn "_dt" ds >>= vToDbl) `orJust` 0.001

unVal = mapE u
  where u (SigVal (Var nm)) = Var $ nm++"Val" 
        u e = e

--stored signals
--parenthesis in pretty printer
--lets in functions