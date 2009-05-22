module ConvertToExpr where

import qualified BNFC.AbsBugpan as B
import Expr
import Numbers
import EvalM
import Data.List 

convertProgram :: B.Program -> [Declare]
convertProgram (B.Prog ds) = map convDecl ds

convDecl (B.DLet (B.Ident nm) e) = Let nm $ cE e
convDecl (B.DType (B.Ident nm) t) = DeclareType nm $cType t
convDecl (B.DSinkConn e idts) = SinkConnect (cE e) idts

unIdent (B.Ident nm) = nm

cE (B.Add e1 e2) = M2 Add (cE e1) (cE e2)
cE (B.Sub e1 e2) = M2 Sub (cE e1) (cE e2)
cE (B.Mul e1 e2) = M2 Mul (cE e1) (cE e2)
cE (B.Div e1 e2) = M2 Div (cE e1) (cE e2)
cE (B.ECmp e1 op e2) = Cmp (cCmpOp op) (cE e1) (cE e2)
cE (B.If p c a) = If (cE p) (cE c) (cE a)
cE (B.Lam (B.PVar (B.Ident nm)) e) = Lam nm $ cE e
cE (B.App e1 e2) = App (cE e1) (cE e2)
cE (B.Pair e1 e2) = Pair (cE e1) (cE e2)
cE (B.Cons e1 e2) = Cons (cE e1) (cE e2)
cE (B.Nil) = Nil
cE (B.Var (B.Ident nm)) = Var nm
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
       (map (\(B.LetLine (B.Ident nm) es) -> (nm, cE es)) les)
       (cE e)
cE (B.ECase e pats) = 
    Case (cE e)
       (map (\(B.CaseLine pat ep) -> (cPat pat, cE ep)) pats)
      

cCmpOp op = case op of
              B.Lt -> Lt
              B.Gt -> Gt
              B.Eq -> Eq
              B.Ne -> Ne
              --B.Lt -> Lt


conToV (B.CInt i) = NumV (NInt $ fromInteger i)
conToV (B.CDbl r) = NumV (NReal r)
conToV B.CUnit = Unit
conToV B.CTrue = BoolV True
conToV B.CFalse = BoolV False

cPat (B.PVar (B.Ident nm)) = PatVar nm
cPat (B.PWild) = PatIgnore
cPat (B.PLit con) = PatLit $ conToV con
cPat (B.PPair p1 p2) = PatPair (cPat p1) (cPat p2)
cPat B.PNil = PatNil
cPat (B.PCons p1 p2) = PatCons (cPat p1) (cPat p2)


cType (B.TUnit) = UnitT

