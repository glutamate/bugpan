module BNFC.SkelBugpan where

-- Haskell module generated by the BNF converter

import BNFC.AbsBugpan
import BNFC.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transBIdent :: BIdent -> Result
transBIdent x = case x of
  BIdent str  -> failure x


transProgram :: Program -> Result
transProgram x = case x of
  Prog bident declares  -> failure x


transDeclare :: Declare -> Result
transDeclare x = case x of
  DLet bident args exp  -> failure x
  DImport bident  -> failure x
  DImportSubst bident impsubstlines  -> failure x
  DType bident type'  -> failure x
  DSinkConn exp0 bident exp  -> failure x
  DReadSrc bident0 bident exp  -> failure x
  DStage bident n  -> failure x
  DStageNeg bident n  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  Add exp0 exp  -> failure x
  Sub exp0 exp  -> failure x
  Mul exp0 exp  -> failure x
  Div exp0 exp  -> failure x
  Negate exp  -> failure x
  Natexp exp  -> failure x
  Natlog exp  -> failure x
  Realpart exp  -> failure x
  Imagpart exp  -> failure x
  EConst const  -> failure x
  And exp0 exp  -> failure x
  Or exp0 exp  -> failure x
  Not exp  -> failure x
  ECmp exp0 cmpop exp  -> failure x
  If exp0 exp1 exp  -> failure x
  Lam pat exp  -> failure x
  App exp0 exp  -> failure x
  Var bident  -> failure x
  Pair exp0 exp  -> failure x
  Pair3 exp0 exp1 exp  -> failure x
  Nil  -> failure x
  Cons exp0 exp  -> failure x
  ListLit exps  -> failure x
  Sig exp  -> failure x
  SigLimited exp0 exp  -> failure x
  SigVal exp  -> failure x
  SigAt exp0 exp  -> failure x
  SigDelay exp0 exp  -> failure x
  Event exp  -> failure x
  Forget exp0 exp  -> failure x
  Switch exp switchlines  -> failure x
  Box exp  -> failure x
  Translate exp0 exp  -> failure x
  Colour exp0 exp  -> failure x
  ELet letlines exp  -> failure x
  ECase exp caselines  -> failure x


transSwitchLine :: SwitchLine -> Result
transSwitchLine x = case x of
  SwitchLine exp0 exp  -> failure x


transLetLine :: LetLine -> Result
transLetLine x = case x of
  LetLine bident exp  -> failure x


transCaseLine :: CaseLine -> Result
transCaseLine x = case x of
  CaseLine pat exp  -> failure x


transArg :: Arg -> Result
transArg x = case x of
  Arg pat  -> failure x


transImpSubstLine :: ImpSubstLine -> Result
transImpSubstLine x = case x of
  ImpSubstLine bident exp  -> failure x


transPat :: Pat -> Result
transPat x = case x of
  PVar bident  -> failure x
  PWild  -> failure x
  PLit const  -> failure x
  PPair pat0 pat  -> failure x
  PNil  -> failure x
  PCons pat0 pat  -> failure x


transConst :: Const -> Result
transConst x = case x of
  CInt n  -> failure x
  CDbl d  -> failure x
  CUnit  -> failure x
  CTrue  -> failure x
  CFalse  -> failure x
  CString str  -> failure x


transCmpOp :: CmpOp -> Result
transCmpOp x = case x of
  Lt  -> failure x
  Gt  -> failure x
  Le  -> failure x
  Ge  -> failure x
  Eq  -> failure x
  Ne  -> failure x


transType :: Type -> Result
transType x = case x of
  TUnit  -> failure x
  TLam type'0 type'  -> failure x
  TPair2 type'0 type'  -> failure x
  TPair3 type'0 type'1 type'  -> failure x
  TNum  -> failure x
  TReal  -> failure x
  TInt  -> failure x
  TBool  -> failure x
  TShape  -> failure x
  TSignal type'  -> failure x
  TEvent type'  -> failure x
  TDuration type'  -> failure x
  TList type'  -> failure x
  TyVar bident  -> failure x



