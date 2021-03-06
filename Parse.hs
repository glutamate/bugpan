module Parse where

import qualified BNFC.AbsBugpan as B
import Expr
import Numbers
import EvalM
import Data.List 
import BNFC.LexBugpan 
import BNFC.ParBugpan
import BNFC.SkelBugpan
import BNFC.PrintBugpan
import BNFC.LayoutBugpan
import BNFC.ErrM
import Data.List.HT (partitionMaybe) 
--import HaskSyntaxUntyped --(splitBySpaces)
import UnitTesting
import PrettyPrint
import Data.Maybe

ident nm=nm


convertProgram :: B.Program -> [Declare]
convertProgram (B.Prog (B.BIdent b) ds) =  map convDecl ds ++ [Let (PatVar "moduleName" StringT) $ Const (StringV b)] 

convDecl (B.DLet pat args e) = Let (cPat pat) . addLamsP (reverse args) $ cE e
convDecl (B.DType (B.BIdent b) t) = DeclareType (ident b) $cType t
convDecl (B.DSinkConn e (B.BIdent b) arg) = SinkConnect (cE e) (b, cE arg)
convDecl (B.DImport (B.BIdent b)) = Import (ident b) [] 
convDecl (B.DImportSubst (B.BIdent b) substs) = Import (ident b) $ map unSubst substs

convDecl (B.DReadSrc (B.BIdent b) (B.BIdent nm) arg ) = ReadSource (ident b) (nm, cE arg)

convDecl (B.DStage (B.BIdent b) si) = Stage (ident b) $ fromInteger si
convDecl (B.DStageNeg (B.BIdent b) nsi) = Stage (ident b) . negate $ fromInteger nsi
convDecl (B.DDist pat diste) = Distribute (cPat pat) (cE diste)
convDecl (B.DEvery pat src decls) = Every (cPat pat) (cE src) $ map convDecl decls
convDecl (B.DPragma (B.BIdent nm) e) = Pragma (ident nm) (cE e) 
--convDecl b = error $"convDecl: "++show b

unSubst (B.ImpSubstLine (B.BIdent b) e) = (ident b, cE e)

addLamsP [] e = e
addLamsP (B.Arg (B.PVar (B.BIdent b)):vs) e = addLamsP vs $ Lam (ident b) UnspecifiedT e

unIdent (B.BIdent b) = (ident b)

cE (B.Add e1 e2) = M2 Add (cE e1) (cE e2)
cE (B.Sub e1 e2) = M2 Sub (cE e1) (cE e2)
cE (B.Mul e1 e2) = M2 Mul (cE e1) (cE e2)
cE (B.Div e1 e2) = M2 Div (cE e1) (cE e2)
cE (B.Negate (B.EConst (B.CInt i))) = Const . NumV $ NInt (negate . fromInteger $ i)
cE (B.Negate (B.EConst (B.CDbl d))) = Const . NumV . NReal $ (negate d)
cE (B.Negate e) = M2 Mul (-1) $ cE e
cE (B.And e1 e2) = And (cE e1) (cE e2)
cE (B.Natexp e1) = M1 Exp (cE e1)
cE (B.Natlog e1) = M1 Ln (cE e1)
cE (B.Realpart e1) = M1 Re (cE e1)
cE (B.Imagpart e1) = M1 Im (cE e1)

cE (B.Or e1 e2) = Or (cE e1) (cE e2)
cE (B.Not e2) = Not (cE e2)
cE (B.ECmp e1 op e2) = Cmp (cCmpOp op) (cE e1) (cE e2)
cE (B.If p c a) = If (cE p) (cE c) (cE a)
cE (B.Lam (B.PVar (B.BIdent b)) e) = Lam (ident b) UnspecifiedT $ cE e
cE (B.Lam (B.PWild) e) = Lam "_" UnspecifiedT $ cE e

cE (B.App e1 e2) = App (cE e1) (cE e2)

cE (B.Pair e1 e2) = Pair (cE e1) (cE e2)
cE (B.Pair3 e1 e2 e3 ) = Pair (Pair (cE e1) (cE e2)) (cE e3)
cE (B.Cons e1 e2) = Cons (cE e1) (cE e2)
cE (B.Nil) = Nil
cE (B.Var (B.BIdent b)) = Var (ident b)
cE (B.EConst c) = Const (conToV c)

cE (B.ListLit []) = Nil 
cE (B.ListLit (e:es)) = Cons (cE e) (cE $ B.ListLit es)

cE (B.Sig e) = Sig $ cE e
cE (B.SigLimited e1 e2) = SigLimited (cE e1) (cE e2)
cE (B.SigVal e) = SigVal $ cE e
cE (B.Event e) = Event $ cE e
cE (B.ETest e1 e2) = ETest (cE e1) (cE e2)
cE (B.EScan e1 e2) = EScan (cE e1) (cE e2)
cE (B.Forget e1 e2) = Forget (cE e1) (cE e2)
cE (B.SigDelay e1 e2) = SigDelay (cE e1) (cE e2)
cE (B.SigAt e1 e2) = SigAt (cE e2) (cE e1)
cE (B.Switch s1 ses) = 
    Switch 
       (map (\(B.SwitchLine ee es) -> (cE ee, cE es)) ses)
       (cE s1)


cE (B.ELet les e) = 
    LetE
       (map (\(B.DLet pat _ es) -> (cPat pat, cE es)) les)
       (cE e)
cE (B.ECase e pats) = 
    Case (cE e)
       (map (\(B.CaseLine pat ep) -> (cPat pat, cE ep)) pats)
      
cE (B.Box e) = Box (cE e)
cE (B.Translate e1 e2) = Translate (cE e1) (cE e2)
cE (B.Colour e1 e2) = Colour (cE e1) (cE e2)
cE (B.SigFby e1 e2) = SigFby (cE e1) (cE e2)
cE e = error $"cE unknown expr: "++show e

cCmpOp op = case op of
              B.Lt -> Lt
              B.Gt -> Gt
              B.Eq -> Eq
              B.Ne -> Ne
              B.Le -> Lt
              B.Ge -> Ge


conToV (B.CInt i) = NumV (NInt $ fromInteger i)
conToV (B.CDbl r) = NumV (NReal $ r)
conToV B.CUnit = Unit
conToV B.CTrue = BoolV True
conToV B.CFalse = BoolV False
conToV (B.CString s) = StringV s

cPat (B.PVar (B.BIdent b)) = PatVar (ident b) UnspecifiedT
cPat (B.PWild) = PatIgnore
cPat (B.PLit con) = PatLit $ conToV con 
cPat (B.PPair p1 p2) = PatPair (cPat p1) (cPat p2)
cPat B.PNil = PatNil
cPat (B.PCons p1 p2) = PatCons (cPat p1) (cPat p2)
cPat (B.PDeriv p) = PatDeriv (cPat p)
cPat (B.PRemember p) = PatRemember (cPat p)

cType (B.TUnit) = UnitT
cType (B.TNum) = NumT Nothing
cType (B.TInt) = NumT (Just IntT)
cType (B.TReal) = NumT (Just RealT)
cType (B.TPair2 t1 t2) = PairT (cType t1) (cType t2)
cType (B.TPair3 t1 t2 t3) = PairT (PairT (cType t1) (cType t2)) (cType t3)
cType (B.TLam t1 t2) = LamT (cType t1) (cType t2)
cType (B.TShape) = ShapeT
cType (B.TBool) = BoolT
cType (B.TList t1) = ListT (cType t1)
cType (B.TSignal t1) = SignalT (cType t1)
cType (B.TEvent t1) = EventT (cType t1)
cType (B.TDuration t1) = DurationT (cType t1)
cType (B.TyVar (B.BIdent tv)) = TyVar tv 


processImports :: [Declare] -> IO [Declare]
processImports ds = 
    let importNm (Import nm subs) = Just (nm, subs)
        importNm _ = Nothing
        (impNms, prog) = partitionMaybe importNm ds
    in do extraDs <- mapM (\(nm, sub) ->fileDecls nm sub) impNms
          return $ (concat extraDs)++prog

fileDecls fnm' subs = do
  let fnm = if ".bug" `isSuffixOf` fnm'
                      then fnm'
                      else fnm'++".bug"
  conts <- readFile $ fnm
  case pProgram $ myLLexer conts of 
    Bad s -> fail $ fnm++": "++s++"\nfile name: \n"++fnm++"\nfile contents: \n"++conts
    Ok ast -> onlyOneModuleName `fmap` ( processImports . makeSubs subs $ convertProgram ast)

stringDecls conts subs = do
  case pProgram $ myLLexer conts of 
    Bad s -> fail $ "stringDecls: "++s++"\nstring contents: \n"++conts
    Ok ast -> onlyOneModuleName `fmap` ( processImports . makeSubs subs $ convertProgram ast)
                

onlyOneModuleName :: [Declare] -> [Declare]
onlyOneModuleName ds = let isModNm (Let (PatVar "moduleName" _) (Const (StringV nm))) = Just nm
                           isModNm _ = Nothing
                           (modNms, otherDs ) = partition (isJust . isModNm) ds
                       in otherDs ++ [last modNms]

makeSubs :: [(String, E)] -> [Declare] -> [Declare] --make substitutions
makeSubs [] ds = ds
makeSubs ((nm,e):subs) ds = makeSubs subs $ makeSub ds
    where makeSub [] = [Let (PatVar nm UnspecifiedT) e]
          makeSub (l@(Let (PatVar nm' t) e'):ds) | nm' == nm = (Let (PatVar nm t) e):ds
                                                 | otherwise = l:makeSub ds
                        
          makeSub (d:ds) = d:makeSub ds


--from TestBugPan.hs, generated  by BNFC

myLLexer = resolveLayout True . myLexer


parsesTo :: String -> E -> Test
parsesTo s e = case pExp $ myLLexer s of
                 Bad err -> return . Just $ "parse fail on "++s++": "++err
                 Ok expr -> if cE expr==e
                               then return Nothing
                               else return . Just $ "parse \""++s++"\" got "++printTree expr++", expected "++pp e

progParsesTo :: String -> [Declare] -> Test
progParsesTo s ds = case pProgram $ myLLexer s of
                      Bad err -> return . Just $ "parse fail on "++s++": "++err
                      Ok expr -> if convertProgram expr==ds
                                  then return Nothing
                                  else return . Just $ "parse \""++s++"\" got "++printTree expr++", expected "++concatMap ppDecl ds


test_parse = ["2+2" `parsesTo` (2+3),
             "(1,2)" `parsesTo`(Pair 1 2),
             "(1,2,3)" `parsesTo` (Pair (Pair 1 2) 3)]

--test_no_semicolons = "let x = 1\nlet y=2\n" `progParsesTo` [Let "x" 1, Let "y" 2]

test_parse_negnums = ["-1" `parsesTo` (-1),
                      "(1,-2)" `parsesTo`(Pair 1 (-2)),
                      -- "-(1+1)" `parsesTo` (-2)),
                      "5*(-1.1)" `parsesTo`(5*(-1.1))]
