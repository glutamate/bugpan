{-# OPTIONS -fno-warn-incomplete-patterns #-}
module BNFC.PrintBugpan where

-- pretty-printer generated by the BNF converter

import BNFC.AbsBugpan
import Char

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print BIdent where
  prt _ (BIdent i) = doc (showString i)
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString " ") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   Prog bident declares -> prPrec i 0 (concatD [doc (showString "module") , prt 0 bident , doc (showString "where") , doc (showString "{") , prt 0 declares , doc (showString "}")])


instance Print Declare where
  prt i e = case e of
   DLet pat args exp -> prPrec i 0 (concatD [prt 0 pat , prt 0 args , doc (showString "=") , prt 0 exp])
   DImport bident -> prPrec i 0 (concatD [doc (showString "use") , prt 0 bident])
   DImportSubst bident impsubstlines -> prPrec i 0 (concatD [doc (showString "use") , prt 0 bident , doc (showString "{") , prt 0 impsubstlines , doc (showString "}")])
   DType bident type' -> prPrec i 0 (concatD [prt 0 bident , doc (showString "::") , prt 0 type'])
   DSinkConn exp0 bident exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "*>") , prt 0 bident , prt 0 exp])
   DReadSrc bident0 bident exp -> prPrec i 0 (concatD [prt 0 bident0 , doc (showString "<*") , prt 0 bident , prt 0 exp])
   DStage bident n -> prPrec i 0 (concatD [doc (showString "stage") , prt 0 bident , prt 0 n])
   DStageNeg bident n -> prPrec i 0 (concatD [doc (showString "stage") , prt 0 bident , doc (showString "-") , prt 0 n])
   DEvery pat exp declares -> prPrec i 0 (concatD [doc (showString "inevery") , prt 0 pat , doc (showString "<-") , prt 0 exp , doc (showString "where") , doc (showString "{") , prt 0 declares , doc (showString "}")])
   DDist pat exp -> prPrec i 0 (concatD [prt 0 pat , doc (showString "~") , prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Exp where
  prt i e = case e of
   Add exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "+") , prt 3 exp])
   Sub exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "-") , prt 3 exp])
   Mul exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "*") , prt 4 exp])
   Div exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "/") , prt 4 exp])
   EIn exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "\\") , prt 4 exp])
   Negate exp -> prPrec i 1 (concatD [doc (showString "-") , prt 2 exp])
   Natexp exp -> prPrec i 5 (concatD [doc (showString "exp") , prt 0 exp])
   Natlog exp -> prPrec i 5 (concatD [doc (showString "ln") , prt 0 exp])
   Realpart exp -> prPrec i 5 (concatD [doc (showString "re") , prt 0 exp])
   Imagpart exp -> prPrec i 5 (concatD [doc (showString "im") , prt 0 exp])
   EConst const -> prPrec i 6 (concatD [prt 0 const])
   And exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "&&") , prt 2 exp])
   Or exp0 exp -> prPrec i 1 (concatD [prt 1 exp0 , doc (showString "||") , prt 2 exp])
   Not exp -> prPrec i 2 (concatD [doc (showString "!") , prt 3 exp])
   ECmp exp0 cmpop exp -> prPrec i 1 (concatD [prt 1 exp0 , prt 0 cmpop , prt 2 exp])
   If exp0 exp1 exp -> prPrec i 0 (concatD [doc (showString "if") , prt 0 exp0 , doc (showString "then") , prt 0 exp1 , doc (showString "else") , prt 0 exp])
   Lam pat exp -> prPrec i 2 (concatD [doc (showString "\\") , prt 0 pat , doc (showString "->") , prt 0 exp])
   App exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , prt 5 exp])
   Var bident -> prPrec i 6 (concatD [prt 0 bident])
   Pair exp0 exp -> prPrec i 4 (concatD [doc (showString "(") , prt 0 exp0 , doc (showString ",") , prt 0 exp , doc (showString ")")])
   Pair3 exp0 exp1 exp -> prPrec i 4 (concatD [doc (showString "(") , prt 0 exp0 , doc (showString ",") , prt 0 exp1 , doc (showString ",") , prt 0 exp , doc (showString ")")])
   Nil  -> prPrec i 6 (concatD [doc (showString "[]")])
   Cons exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString ":") , prt 0 exp])
   ListLit exps -> prPrec i 4 (concatD [doc (showString "[") , prt 0 exps , doc (showString "]")])
   Sig exp -> prPrec i 1 (concatD [doc (showString "{:") , prt 0 exp , doc (showString ":}")])
   SigLimited exp0 exp -> prPrec i 1 (concatD [doc (showString "{:") , prt 0 exp0 , doc (showString "#") , prt 0 exp , doc (showString ":}")])
   SigVal exp -> prPrec i 6 (concatD [doc (showString "<:") , prt 0 exp , doc (showString ":>")])
   SigAt exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "@") , prt 3 exp])
   SigDelay exp0 exp -> prPrec i 4 (concatD [doc (showString "delay") , prt 4 exp0 , prt 5 exp])
   SigDeriv exp -> prPrec i 2 (concatD [doc (showString "D") , prt 3 exp])
   SigFby exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "fby") , prt 3 exp])
   Event exp -> prPrec i 4 (concatD [doc (showString "[:") , prt 0 exp , doc (showString ":]")])
   ETest exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "?") , prt 3 exp])
   EScan exp0 exp -> prPrec i 4 (concatD [doc (showString "escan") , prt 4 exp0 , prt 5 exp])
   Forget exp0 exp -> prPrec i 4 (concatD [doc (showString "forget") , prt 4 exp0 , prt 5 exp])
   Switch exp switchlines -> prPrec i 0 (concatD [doc (showString "switch") , doc (showString "{") , prt 0 exp , doc (showString ";") , prt 0 switchlines , doc (showString "}")])
   Box exp -> prPrec i 5 (concatD [doc (showString "box") , prt 4 exp])
   Translate exp0 exp -> prPrec i 4 (concatD [doc (showString "translate") , prt 4 exp0 , prt 5 exp])
   Colour exp0 exp -> prPrec i 4 (concatD [doc (showString "colour") , prt 4 exp0 , prt 5 exp])
   ELet declares exp -> prPrec i 0 (concatD [doc (showString "let") , doc (showString "{") , prt 0 declares , doc (showString "}") , doc (showString "in") , prt 0 exp])
   ECase exp caselines -> prPrec i 0 (concatD [doc (showString "case") , prt 0 exp , doc (showString "of") , doc (showString "{") , prt 0 caselines , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print SwitchLine where
  prt i e = case e of
   SwitchLine exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString "~>") , prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print CaseLine where
  prt i e = case e of
   CaseLine pat exp -> prPrec i 0 (concatD [prt 0 pat , doc (showString "->") , prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Arg where
  prt i e = case e of
   Arg pat -> prPrec i 0 (concatD [prt 1 pat])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print ImpSubstLine where
  prt i e = case e of
   ImpSubstLine bident exp -> prPrec i 0 (concatD [prt 0 bident , doc (showString "=>") , prt 0 exp])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Pat where
  prt i e = case e of
   PVar bident -> prPrec i 1 (concatD [prt 0 bident])
   PWild  -> prPrec i 1 (concatD [doc (showString "_")])
   PLit const -> prPrec i 1 (concatD [prt 0 const])
   PPair pat0 pat -> prPrec i 1 (concatD [doc (showString "(") , prt 0 pat0 , doc (showString ",") , prt 0 pat , doc (showString ")")])
   PNil  -> prPrec i 1 (concatD [doc (showString "[]")])
   PCons pat0 pat -> prPrec i 0 (concatD [prt 0 pat0 , doc (showString ":") , prt 0 pat])
   PIn pat0 pat -> prPrec i 0 (concatD [prt 0 pat0 , doc (showString "\\") , prt 0 pat])
   PDeriv pat -> prPrec i 0 (concatD [doc (showString "D") , prt 0 pat])
   PRemember pat -> prPrec i 0 (concatD [prt 0 pat , doc (showString "!")])


instance Print Const where
  prt i e = case e of
   CInt n -> prPrec i 0 (concatD [prt 0 n])
   CDbl d -> prPrec i 0 (concatD [prt 0 d])
   CUnit  -> prPrec i 0 (concatD [doc (showString "()")])
   CTrue  -> prPrec i 0 (concatD [doc (showString "true")])
   CFalse  -> prPrec i 0 (concatD [doc (showString "false")])
   CString str -> prPrec i 0 (concatD [prt 0 str])


instance Print CmpOp where
  prt i e = case e of
   Lt  -> prPrec i 0 (concatD [doc (showString "<")])
   Gt  -> prPrec i 0 (concatD [doc (showString ">")])
   Le  -> prPrec i 0 (concatD [doc (showString "<=")])
   Ge  -> prPrec i 0 (concatD [doc (showString ">=")])
   Eq  -> prPrec i 0 (concatD [doc (showString "==")])
   Ne  -> prPrec i 0 (concatD [doc (showString "!=")])


instance Print Type where
  prt i e = case e of
   TUnit  -> prPrec i 2 (concatD [doc (showString "()")])
   TLam type'0 type' -> prPrec i 1 (concatD [prt 2 type'0 , doc (showString "->") , prt 1 type'])
   TPair2 type'0 type' -> prPrec i 2 (concatD [doc (showString "(") , prt 0 type'0 , doc (showString ",") , prt 0 type' , doc (showString ")")])
   TPair3 type'0 type'1 type' -> prPrec i 2 (concatD [doc (showString "(") , prt 0 type'0 , doc (showString ",") , prt 0 type'1 , doc (showString ",") , prt 0 type' , doc (showString ")")])
   TNum  -> prPrec i 2 (concatD [doc (showString "Number")])
   TReal  -> prPrec i 2 (concatD [doc (showString "Real")])
   TInt  -> prPrec i 2 (concatD [doc (showString "Int")])
   TBool  -> prPrec i 2 (concatD [doc (showString "Bool")])
   TShape  -> prPrec i 2 (concatD [doc (showString "Shape")])
   TSignal type' -> prPrec i 2 (concatD [doc (showString "Signal") , prt 0 type'])
   TEvent type' -> prPrec i 1 (concatD [doc (showString "Event") , prt 2 type'])
   TDuration type' -> prPrec i 1 (concatD [doc (showString "Duration") , prt 2 type'])
   TList type' -> prPrec i 2 (concatD [doc (showString "[") , prt 0 type' , doc (showString "]")])
   TyVar bident -> prPrec i 2 (concatD [prt 0 bident])



