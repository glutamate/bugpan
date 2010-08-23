module GenC.Backend where

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
import GenC.Syntax
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
    mapM (putStrLn . ppDecl)  ds
  let prg = ppCProg $ toC dt tmax ds params
  writeFile (fp) prg
  --putStrLn prg 
  return ()

gloDbl nm x=DeclareGlobal CDoubleT nm (Just (Const (NumV $ NReal x))) 

toC dt tmax ds params
    = let (env:stageDs) = splitByStages ds
          stages = zip [0..] stageDs
      in concat [imports ds, 
                 [gloDbl "dt" dt, gloDbl "tmax" tmax], 
                 globals ds, 
                 concatMap stepper stages, 
                 [mainFun params ds stages]]

mainFun params ds stages
    = CFun CIntT "main" [("argc", CIntT), 
                             ("argv", CPtrT $ CPtrT CCharT)] $
               [Assign (Var "npnts") $ Var "llround" $> (Var "tmax"/Var"dt")]++  
               map setPar (zip params [1..])++
               concat(nub $ map (mainBeg ds) ds)++
               concatMap runStage stages++
               concatMap (mainEnd ds) ds++
               [Return 1]

setPar ((nm, NumT (Just RealT)), n)
       = Call "sscanf" [Var ("argv["++show n++"]"), Const (StringV "%lf"), Var ("&"++nm)]

runStage (n, ds) = {- (traceS $"stage"++show n )++ -} concatMap runAtOnceSrcs ds++concatMap preStage ds++ driveStage (n, ds) 

isDynClamp ds = 
    let ins = [() | ReadSource _ ("ADC",_) <- ds ]
        outs = [() | SinkConnect _ ("DAC",_) <- ds ]
    in not (null ins || null outs)

driveStage (n, ds) 
    | isDynClamp ds = [Call ("stepdyn"++show n) []]
    | otherwise = [forCount "i" 0 (Var "npnts") [Call ("step"++show n) []]]

runAtOnceSrcs (ReadSource vnm ("poisson", rate)) 
         = [Assign (Var vnm) (Var "poisson_train" $> rate $> Var "tmax")]
runAtOnceSrcs _ = []
preStage (Let (PatVar nm t) (Forget tm (Var es))) 
         = [Assign (Var nm) (Var es)]
preStage _ = []

imports ds 
        | isDynClamp ds =std ++ [CInclude False "dyncomedi.c"]
        | otherwise = std
  where std = map (CInclude True) ["stdlib.h","stdio.h", "math.h"] ++ 
             [CInclude False "dynprelude.c"]                                       

mainBeg ds (SinkConnect (Var nm) ("store", _)) = 
        let t =tyOf ds nm in
        [Assign (Var $ nm)
                (Var "create_sig" $> Var "npnts" $> Var "dt")]
                --(Var "malloc" $> (Var "npnts" * (Var "sizeof" $> Var (ppCTy $ bugTyToCTy t)))), 
mainBeg ds (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var $ nm)
                (Var "create_sig" $> Var "npnts"$> Var "dt")]
                                       | otherwise = []

         
mainBeg ds (Let (PatVar nm t) e) 
                    | "_0" `isSuffixOf` nm = [Assign (Var ((dropEnd 2 nm)++"Val")) e]
                    | otherwise = []
mainBeg _ _ = []

dropEnd n = reverse . drop n . reverse

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
gloVar _ (Let (PatVar nm t) (SolveOde e)) = 
                    [DeclareGlobal (bugTyToCTy t) (nm++"Val") (Nothing)]
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

stepper (stage,ds) = [CFun VoidT ("step"++show stage) [] $ (secs:(concat$ nub $map step ds))++tr]
                     ++dynStepper (stage,ds)
    where secs = DecVar CDoubleT "secondsVal" (Just $ Var "i"*Var "dt")
          tr = [] -- [traceD ("step"++show stage) "secondsVal" ]
step (Let (PatVar nm t) (Sig e)) = [Assign (Var (nm++"Val")) $ unVal e]
step (Let (PatVar nm t) (Forget tm e)) = 
          [Assign (Var (nm)) (Var "forget_events" $> Var nm $> (Var "secondsVal"- tm))]
--step (SinkConnect (Var nm) ("store", _)) = [Assign (Var ("("++nm++"->arr)[i]")) $ Var  (nm++"Val")]
step (Let (PatVar nm t) (SolveOde (SigFby v e))) = 
                    [Assign (Var (nm++"Val")) $ (Var (nm++"Val")) + Var "dt" * unVal (SigVal e)]
step (Let (PatVar nm t) (SolveOde (Sig e))) = 
                    [Assign (Var (nm++"Val")) $ (Var (nm++"Val")) + Var "dt" *  e]
step (SinkConnect (Var nm) (bufnm, _)) | head bufnm == '#' = 
             [Assign (Var ("("++nm++"->arr)[i]")) $ Var (nm++"Val")]
                                       | otherwise = []
step d = []

dynStepper (stage,ds) 
    | isDynClamp ds = [CFun CIntT ("stepdyn"++show stage) [] $ dynBegin ds++[dynLoop ds]++dynEnd ds]
    | otherwise = []
 
dynBegin ds = 
    let headOr x [] = x
        headOr _ (x:_) = x
        chanIn = headOr "0" [pp chan | ReadSource _ ("ADC",chan) <- ds ] 
        chanOut = headOr "0" [pp chan | SinkConnect _ ("DAC",chan) <- ds ] in 
    [LitCmds 
       ["RTIME until;",
        "RTIME samp_time=dt*1000*1000*1000;",
	"RT_TASK *task;",
	"comedi_insn insn_read;",
	"comedi_insn insn_write;",
	"//lsampl_t *hist;",
	"lsampl_t data[NCHAN];",
        "double secondsVal, tnow, tlast, max_t_diff=0;",
	"signal(SIGKILL, endme);",
	"signal(SIGTERM, endme);",
	"//hist = malloc(SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);",
	"//memset(hist, 0, SAMP_FREQ*RUN_TIME*NCHAN*sizeof(lsampl_t) + 1000);",

	"start_rt_timer(0);",
	"task = rt_task_init_schmod(nam2num(\"MYTASK\"), 1, 0, 0, SCHED_FIFO, 0xF);",
	
	"mlockall(MCL_CURRENT | MCL_FUTURE);",
	"rt_make_hard_real_time();", 
	"if (init_board()) {;",
	"       printf(\"Board initialization failed.\\n\");",
	"       return 1;",
	"}",
	"BUILD_AREAD_INSN(insn_read, subdevai, data[0], 1, "++chanIn++" , AI_RANGE, AREF_GROUND);",
	"BUILD_AWRITE_INSN(insn_write, subdevao, data[1], 1, "++chanOut++", AO_RANGE, AREF_GROUND);",
        "data[1] = from_phys(0);",
        "until = rt_get_time();",
        "tlast = count2nano(until);"]]

dynLoop ds =
    For (Assign (Var "i") 0) (And (Cmp Lt (Var "i") (Var "npnts")) (Var "!end")) (Assign (Var "i") (Var "i"+1)) $ 
            [LitCmds ["secondsVal=i*dt;",
                      "comedi_do_insn(dev,&insn_read);"]]
            ++(concat$ nub $map stepd ds)++
            [LitCmds ["comedi_do_insn(dev,&insn_write);",
                      "tnow = count2nano(rt_get_time());",
                      "if(tnow-tlast>max_t_diff) max_t_diff = tnow-tlast;",
                      "tlast= tnow;",
                      "rt_sleep_until(until += nano2count(samp_time));"]]
	
dynEnd ds= 
    let headOr x [] = x
        headOr _ (x:_) = x
        chanIn = headOr "0" [pp chan | ReadSource _ ("ADC",chan) <- ds ] 
        chanOut = headOr "0" [pp chan | SinkConnect _ ("DAC",chan) <- ds ] in 
    [LitCmds 
       ["comedi_cancel(dev, subdevai);",
	"comedi_cancel(dev, subdevao);",
	"comedi_data_write(dev, subdevao, "++chanOut++", 0, AREF_GROUND, from_phys(0));",
	"//comedi_data_write(dev, subdevao, 1, 0, AREF_GROUND, 2048);",
	"comedi_close(dev);"],
    LitCmds 
       ["//free(hist);",
	"stop_rt_timer();",
	"rt_make_soft_real_time();",
	"rt_task_delete(task);",
        "printf(\"largest time difference %g ns\\n\", max_t_diff);",
        "return 0;"]]

stepd (SinkConnect (Var nm) ("DAC", _)) = [Assign  (Var "data[1]") (Var "from_phys" $> Var (nm++"Val")) ]
stepd (ReadSource nm ("ADC", _)) = [Assign (Var (nm++"Val")) $ Var "to_phys" $> Var "data[0]"]
stepd d = step d

traceD what nm = [Call "printf" [Const (StringV $ what ++ " " ++ nm ++" %g\n"), Var nm], Call "fflush" [Var "stdout"]]
traceInt what nm = [Call "printf" [Const (StringV $ what ++ " " ++ nm ++" %ld\n"), Var nm], Call "fflush" [Var "stdout"]]
traceS what= [Call "printf" [Const (StringV $ what++"\n")], Call "fflush" [Var "stdout"]]


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
