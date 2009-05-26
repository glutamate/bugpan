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
--import BNFC.LayoutBugpan
import BNFC.ErrM
import Data.List.HT (partitionMaybe)
import HaskSyntaxUntyped (splitBySpaces)

ident nm=nm


convertProgram :: B.Program -> [Declare]
convertProgram (B.Prog ds) = map convDecl ds

convDecl (B.DLet (B.BIdent b) args e) = Let (ident b) . addLamsP (reverse args) $ cE e
convDecl (B.DType (B.BIdent b) t) = DeclareType (ident b) $cType t
convDecl (B.DSinkConn e idts) = SinkConnect (cE e) idts
convDecl (B.DImport (B.BIdent b)) = Import (ident b) [] 
convDecl (B.DImportSubst (B.BIdent b) substs) = Import (ident b) $ map unSubst substs
convDecl (B.DReadSrc (B.BIdent b) spec) = ReadSource (ident b) $ splitBySpaces spec
convDecl (B.DStage (B.BIdent b) intStr) = Stage (ident b) $ read intStr
--convDecl b = error $"convDecl: "++show b

unSubst (B.ImpSubstLine (B.BIdent b) e) = (ident b, cE e)

addLamsP [] e = e
addLamsP (B.Arg (B.PVar (B.BIdent b)):vs) e = addLamsP vs $ Lam (ident b) e

unIdent (B.BIdent b) = (ident b)

cE (B.Add e1 e2) = M2 Add (cE e1) (cE e2)
cE (B.Sub e1 e2) = M2 Sub (cE e1) (cE e2)
cE (B.Mul e1 e2) = M2 Mul (cE e1) (cE e2)
cE (B.Div e1 e2) = M2 Div (cE e1) (cE e2)
cE (B.And e1 e2) = And (cE e1) (cE e2)
cE (B.Natexp e1) = M1 Exp (cE e1)
cE (B.Natlog e1) = M1 Ln (cE e1)
cE (B.Realpart e1) = M1 Re (cE e1)
cE (B.Imagpart e1) = M1 Im (cE e1)

cE (B.Or e1 e2) = Or (cE e1) (cE e2)
cE (B.Not e2) = Not (cE e2)
cE (B.ECmp e1 op e2) = Cmp (cCmpOp op) (cE e1) (cE e2)
cE (B.If p c a) = If (cE p) (cE c) (cE a)
cE (B.Lam (B.PVar (B.BIdent b)) e) = Lam (ident b) $ cE e
cE (B.Lam (B.PWild) e) = Lam "_" $ cE e

cE (B.App e1 e2) = App (cE e1) (cE e2)
cE (B.Pair e1 e2) = Pair (cE e1) (cE e2)
cE (B.Cons e1 e2) = Cons (cE e1) (cE e2)
cE (B.Nil) = Nil
cE (B.Var (B.BIdent b)) = Var (ident b)
cE (B.EConst c) = Const (conToV c)

cE (B.ListLit es) = foldl (Cons) Nil $ map cE es

cE (B.Sig e) = Sig $ cE e
cE (B.SigVal e) = SigVal $ cE e
cE (B.Event e) = Event $ cE e
cE (B.SigDelay e1 e2) = SigDelay (cE e1) (cE e2)
cE (B.SigAt e1 e2) = SigAt (cE e1) (cE e2)
cE (B.Switch s1 ses) = 
    Switch 
       (map (\(B.SwitchLine ee es) -> (cE ee, cE es)) ses)
       (cE s1)


cE (B.ELet les e) = 
    LetE
       (map (\(B.LetLine (B.BIdent b) es) -> ((ident b), cE es)) les)
       (cE e)
cE (B.ECase e pats) = 
    Case (cE e)
       (map (\(B.CaseLine pat ep) -> (cPat pat, cE ep)) pats)
      
cE e = error $"cE: "++show e

cCmpOp op = case op of
              B.Lt -> Lt
              B.Gt -> Gt
              B.Eq -> Eq
              B.Ne -> Ne
              B.Le -> Lt
              B.Ge -> Ge


conToV (B.CInt i) = NumV (NInt $ fromInteger i)
conToV (B.CDbl r) = NumV (NReal r)
conToV B.CUnit = Unit
conToV B.CTrue = BoolV True
conToV B.CFalse = BoolV False

cPat (B.PVar (B.BIdent b)) = PatVar (ident b)
cPat (B.PWild) = PatIgnore
cPat (B.PLit con) = PatLit $ conToV con
cPat (B.PPair p1 p2) = PatPair (cPat p1) (cPat p2)
cPat B.PNil = PatNil
cPat (B.PCons p1 p2) = PatCons (cPat p1) (cPat p2)


cType (B.TUnit) = UnitT

processImports :: [Declare] -> IO [Declare]
processImports ds = 
    let importNm (Import nm _) = Just nm
        importNm _ = Nothing
        (impNms, prog) = partitionMaybe importNm ds
    in do extraDs <- mapM fileDecls impNms
          return $ (concat extraDs)++prog

fileDecls fnm = do
  conts <- readFile $ fnm++".bug"
  case pProgram $ myLLexer conts of 
    Bad s -> fail $ fnm++": "++s
    Ok ast -> processImports $ convertProgram ast
                

--from TestBugPan.hs, generated  by BNFC

myLLexer = {-resolveLayout True .-} myLexer


