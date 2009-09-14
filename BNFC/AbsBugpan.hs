module BNFC.AbsBugpan where

-- Haskell module generated by the BNF converter

newtype BIdent = BIdent String deriving (Eq,Ord,Show)
data Program =
   Prog BIdent [Declare]
  deriving (Eq,Ord,Show)

data Declare =
   DLet BIdent [Arg] Exp
 | DImport BIdent
 | DImportSubst BIdent [ImpSubstLine]
 | DType BIdent Type
 | DSinkConn Exp BIdent Exp
 | DReadSrc BIdent BIdent Exp
 | DStage BIdent Integer
 | DStageNeg BIdent Integer
  deriving (Eq,Ord,Show)

data Exp =
   Add Exp Exp
 | Sub Exp Exp
 | Mul Exp Exp
 | Div Exp Exp
 | Negate Exp
 | Natexp Exp
 | Natlog Exp
 | Realpart Exp
 | Imagpart Exp
 | EConst Const
 | And Exp Exp
 | Or Exp Exp
 | Not Exp
 | ECmp Exp CmpOp Exp
 | If Exp Exp Exp
 | Lam Pat Exp
 | App Exp Exp
 | Var BIdent
 | Pair Exp Exp
 | Pair3 Exp Exp Exp
 | Nil
 | Cons Exp Exp
 | ListLit [Exp]
 | Sig Exp
 | SigLimited Exp Exp
 | SigVal Exp
 | SigAt Exp Exp
 | SigDelay Exp Exp
 | Event Exp
 | Forget Exp Exp
 | Switch Exp [SwitchLine]
 | Box Exp
 | Translate Exp Exp
 | Colour Exp Exp
 | ELet [LetLine] Exp
 | ECase Exp [CaseLine]
  deriving (Eq,Ord,Show)

data SwitchLine =
   SwitchLine Exp Exp
  deriving (Eq,Ord,Show)

data LetLine =
   LetLine BIdent Exp
  deriving (Eq,Ord,Show)

data CaseLine =
   CaseLine Pat Exp
  deriving (Eq,Ord,Show)

data Arg =
   Arg Pat
  deriving (Eq,Ord,Show)

data ImpSubstLine =
   ImpSubstLine BIdent Exp
  deriving (Eq,Ord,Show)

data Pat =
   PVar BIdent
 | PWild
 | PLit Const
 | PPair Pat Pat
 | PNil
 | PCons Pat Pat
  deriving (Eq,Ord,Show)

data Const =
   CInt Integer
 | CDbl Double
 | CUnit
 | CTrue
 | CFalse
 | CString String
  deriving (Eq,Ord,Show)

data CmpOp =
   Lt
 | Gt
 | Le
 | Ge
 | Eq
 | Ne
  deriving (Eq,Ord,Show)

data Type =
   TUnit
 | TLam Type Type
 | TPair2 Type Type
 | TPair3 Type Type Type
 | TNum
 | TReal
 | TInt
 | TBool
 | TShape
 | TSignal Type
 | TEvent Type
 | TDuration Type
 | TList Type
 | TyVar BIdent
  deriving (Eq,Ord,Show)

