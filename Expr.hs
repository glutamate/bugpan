{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Expr where

import Control.Monad
import Control.Monad.Reader
import Numbers
import EvalM
import Data.List
import TNUtils
import Data.Generics

data Math1 = Ln | Exp | Re | Im deriving (Show, Eq, Read, Data, Typeable)
data Math2 = Add | Sub | Mul | Div deriving (Show, Eq, Read, Data, Typeable)
data CmpOp = Lt | Gt | Eq | Ne | Le | Ge deriving (Show, Eq, Read, Data, Typeable)

--data EvalM a = Res {unEvalM :: a} | Error String

--newtype EvalM = Res { ReaderT Eval



instance Num V where
    (NumV n1) + (NumV n2) = NumV $n1+n2
    (NumV n1) - (NumV n2) = NumV $n1-n2
    e1 - e2 = error $ "error: "++show e1++"-"++show e2
    (NumV n1) * (NumV n2) = NumV $n1*n2
    abs (NumV n1) = NumV (abs n1)
    signum (NumV n1) = NumV (signum n1)
    negate (NumV n1) = NumV $ (NInt (-1))*n1
    fromInteger n = NumV . fromInteger $ n

instance Fractional V where
	(NumV n1) / (NumV n2) = NumV $ n1/n2
	fromRational r = NumV . NReal $ fromRational r

 

data E =  If E E E
        | Const V
	| Lam String T E
	| App E E
	| Var String
	| Pair E E
	| Nil 
	| Cons E E
	| M1 Math1 E
	| M2 Math2 E E
	| Cmp CmpOp E E
	| And E E | Or E E | Not E
	| Case E [(Pat,E)]
	| Sig E
	| SigLimited E E
	| SigVal E
	| SigAt E E
        | SigDelay E E
        | SolveOde E
        | Switch [(E,E)] E
        | Event E
        | ETest E E
        | EScan E E
        | Forget E E
        | LetE [(Pat,E)] E
        | Box E
        | Translate E E
        | Colour E E
        | HasType T E
        | SigFby E E
	deriving (Show, Eq, Read, Data, Typeable)

data Declare 
	= Let Pat E
        | DeclareType String T
	| Import String [(String, E)]
	| SinkConnect E (String,E)
        | ReadSource String (String,E)
        | Stage String Int
        | Comment String
        | Nop
	deriving (Show, Eq, Read)


isSubTermIn :: E-> E-> Bool
isSubTermIn small big = not . null $ queryE tst big
    where tst someE | someE == small = [someE]
                    | otherwise = []


flatE :: E -> [E]
flatE = queryE (unitList)
    where unitList x = [x]

gqueryE :: (E-> [a]) -> E -> [a]
gqueryE qf = everything (++) ([] `mkQ` qf)


queryE :: (E-> [a]) -> E -> [a]
queryE = gqueryE

alphaConvert :: E -> E -> E -> E
alphaConvert from to = mapE f
    where f e | e == from = to
              | otherwise = e

gmapE :: (E -> E) -> E -> E
gmapE f = everywhere (mkT f)

mapE :: (E-> E)-> E -> E
mapE = gmapE

unLam :: E -> ([String], E)
unLam e = unLam' e []
unLam' (Lam nm _ bd) nms = unLam' bd (nm:nms)
unLam' e nms = (nms, e)

subVar n es e = mapE f e where 
    f (Var n') | n== n' = es
               | otherwise = Var n'
    f x = x

instance Num E where
	e1 + e2 = M2 Add e1 e2
	e1 - e2 = M2 Sub e1 e2
	e1 * e2 = M2 Mul e1 e2
        negate (Const (NumV nv)) = Const $ NumV (negate nv)
	negate e = M2 Mul (Const . NumV . NInt $ (-1)) e
	abs e = If (Cmp Lt e 0) (negate e) (e)
	signum e = If (Cmp Lt e 0) (-1) (1)
	fromInteger i = Const . NumV $ NInt (fromInteger i)

instance Fractional E where
	e1 / e2 = M2 Div e1 e2
	fromRational r = Const . NumV . NReal $ fromRational r

instance Floating E where
        pi = Const . NumV . NReal $ pi
	exp = M1 Exp 
        log = M1 Ln
        sin = (Var "sin" $>)
        cos = (Var "cos" $>)
        tan = (Var "tan" $>)
        asin = (Var "asin" $>)
        acos = (Var "acos" $>)
        atan = (Var "atan" $>)
        sinh = (Var "sinh" $>)
        cosh = (Var "cosh" $>)
        asinh = (Var "asinh" $>)
        acosh = (Var "acosh" $>)
        atanh = (Var "atanh" $>)

data Pat = 	  PatVar String T
		| PatIgnore
		| PatLit V
		| PatPair Pat Pat
		| PatNil 
		| PatCons Pat Pat
                | PatDeriv Pat
		-- | PatGuard E Pat
		deriving (Show, Eq, Read, Data, Typeable)

unsafePatToName (PatVar nm _) = nm

patIntroducedVars (PatVar nm t) = [nm]
patIntroducedVars (PatCons h t)= patIntroducedVars h ++ patIntroducedVars t
patIntroducedVars (PatPair h t)= patIntroducedVars h ++ patIntroducedVars t
patIntroducedVars _ = []


--sugar

($>) = App


infixl 6 .>.
infixl 6 .<.
infixl 6 .>=.
infixl 6 .<=.
infixl 5 .&.
infixl 4 .|.


e1 .>. e2 = Cmp Gt e1 e2
e1 .<. e2 = Cmp Lt e1 e2
e1 .>=. e2 = Cmp Ge e1 e2
e1 .<=. e2 = Cmp Le e1 e2
e1 .==. e2 = Cmp Eq e1 e2




e1 .&. e2 = And e1 e2
e1 .|. e2 = Or e1 e2

lookupDefn _ [] = Nothing
lookupDefn nm ((Let (PatVar nm' _) (Const v)):ds) | nm == nm' = Just $ v
                                                  | otherwise = lookupDefn nm ds
lookupDefn nm (d:ds) = lookupDefn nm ds


