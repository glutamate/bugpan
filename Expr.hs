{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Control.Monad
import Control.Monad.Reader
import Numbers
import EvalM
import Data.List
--import Types

data Math1 = Ln | Exp | Re | Im deriving (Show, Eq, Read)
data Math2 = Add | Sub | Mul | Div deriving (Show, Eq, Read)
data CmpOp = Lt | Gt | Eq | Ne | Le | Ge deriving (Show, Eq, Read)

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

 

data E =  If E E E
        | Const V
	| Lam String E
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
	| SigVal E
	| SigAt E E
        | SigDelay E E
        | Switch [(E,E)] E
        | Event E
        | LetE [(String, E)] E
        | Box E
        | Translate E E
        | Colour E E
        | HasType T E
	deriving (Show, Eq, Read)

data Declare 
	= Let String E
        | DeclareType String T
--	| LetEvt String E
--	| LetRec String E
	| Import String
	| SinkConnect E String
        | ReadSource String [String]
        | Stage String Int
        | Nop
	deriving (Show, Eq, Read)

ppDecl (Let nm e ) = nm++" = " ++ pp e
ppDecl (SinkConnect e sn) = (pp e++" *> " ++ sn)
ppDecl (ReadSource varNm srcNm) = (varNm++" <- " ++ (intercalate " " srcNm))
ppDecl s = show s

--for display purposes only
depth :: E->Int
depth (Const _) = 1
depth (Lam _ b) = 1+depth b
depth (App f b) = 1+ max (depth f) (depth b)
depth (Var _) = 1
depth (Pair e1 e2) = 1+ max (depth e1) (depth e2)
depth (Nil) = 1
depth (Cons e1 Nil) = 1
depth (Cons e1 e2) = 1+max (depth e1) (depth e2)+1
depth (M2 _ e1 e2) = 1+max (depth e1) (depth e2)+1
depth (M1 _ e1) = 1+(depth e1) 
depth (Sig _) = 1
depth (SigVal _) = 1
depth _ = 2

ppa :: E-> String
ppa p@(Pair _ _) = pp p 
ppa e | depth e > 1 = "("++pp e++")"
      | otherwise = pp e

pp :: E->String
pp (If p c a) = concat ["if ", pp p, " then ", ppa c, " else ", ppa a]
pp (Lam n e) = concat ["\\", n, "->", pp e]
pp (Var n) = n
pp (Const v) = show v
pp (App f a) = ppa f ++ " " ++ ppa a
pp (Pair (Pair x y) z) = concat ["(", pp x , ", ", pp y,", ", pp z, ")"]
pp (Pair f s) = concat ["(", pp f , ", ", pp s, ")"]
pp (Nil) = "[]"
pp (Cons car Nil) = "[ "++pp car++" ]" -- ppa car ++ ":" ++ ppa cdr
pp (Cons car cdr) = ppa car ++ ":" ++ ppa cdr
pp (Cmp Lt e1 e2) = ppa e1 ++ " < " ++ ppa e2
pp (Cmp Gt e1 e2) = ppa e1 ++ " > " ++ ppa e2
pp (Cmp Eq e1 e2) = ppa e1 ++ " == " ++ ppa e2
pp (Cmp Ne e1 e2) = ppa e1 ++ " != " ++ ppa e2
pp (Cmp Le e1 e2) = ppa e1 ++ " <= " ++ ppa e2
pp (Cmp Ge e1 e2) = ppa e1 ++ " >= " ++ ppa e2
pp (And e1 e2) = ppa e1 ++ " && " ++ ppa e2
pp (Or e1 e2) = ppa e1 ++ " || " ++ ppa e2
pp (Not e1) = "!" ++ ppa e1
pp (Sig e) = "{: "++pp e++" :}"
pp (SigVal s) = "<: "++pp s++" :>"
pp (SigAt t s) = ppa s ++ "@" ++ ppa t
pp (SigDelay s v) = "delay "++ppa s++" "++ppa v
pp (Switch swsgs sig1) = "switch {\n"++ ppa sig1 ++"; \n" ++ passocs ++ "}"
    where passocs = concatMap (\(e,slam)-> pp e ++ " ~> " ++ pp slam++";\n") swsgs
pp (Event e) = "[: "++pp e++" :]"
pp (M2 Mul e1 e2) = pp2op e1 "*" e2
pp (M2 Add e1 e2) = pp2op e1 "+" e2
pp (M2 Sub (Const (NumV (NInt 0))) e2) = "-" ++ppa e2
pp (M2 Sub e1 e2) = pp2op e1 "-" e2
pp (M2 Div e1 e2) = pp2op e1 "/" e2
pp (M1 op e) = show op ++ " " ++ ppa e
pp (LetE les efinal) = concat ["let ", concat $ ppes les, " in ", ppa efinal]
    where ppes es = map (\(n,e)-> n++" = "++pp e++";") es 
pp (Box d) = "cube "++ppa d
pp (Translate t e) = "translate "++ppa t++" "++ppa e
pp (Colour t e) = "colour "++ppa t++" "++ppa e
pp (Case tst pats) = "case "++pp tst++" of "++ concatMap (\(pat,e)-> ppPat pat++" -> "++pp e++"; ") pats
pp e = show e

pp2op e1 op e2 = ppa e1 ++ op ++ ppa e2

isSubTermIn :: E-> E-> Bool
isSubTermIn small big = not . null $ queryE tst big
    where tst someE | someE == small = [someE]
                    | otherwise = []

queryE :: (E-> [a]) -> E -> [a]
queryE q e@(If p c a) = q e ++ m p ++ m c ++m a
	where m = queryE q

queryE q e@(LetE ses er) = q e ++ m er ++ concatMap (m . snd) ses  
	where m = queryE q
queryE q e@(Switch ses er) = concat [q e, m er, 
                                     concatMap (m . fst) ses,
                                     concatMap (m . snd) ses]
	where m = queryE q

queryE q e@(Lam n bd) = q e ++ m bd
	where m = queryE q
queryE q e@(App le ae) = q e ++ m le ++ m ae
	where m = queryE q
queryE q e@(Pair e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Cons e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(M1 _ e1) = q e++m e1
	where m = queryE q
queryE q e@(M2 _ e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Cmp _ e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(And e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Or e1 e2) = q e++m e1 ++ m e2
	where m = queryE q
queryE q e@(Not e1) = q e++m e1
	where m = queryE q
queryE q e@(Sig e1) = q e++m e1
	where m = queryE q
queryE q e@(SigVal e1) = q e++m e1
	where m = queryE q
queryE q e@(SigDelay e1 e2) = q e++m e1++ m e2
	where m = queryE q
queryE q e@(Event e1) = q e++m e1
	where m = queryE q
queryE q e@(Const _) = q e
	where m = queryE q
queryE q e@(SigAt e1 e2) = q e++m e1++m e2
	where m = queryE q
queryE q e@(Case ce cs) = q e++q ce++concatMap (m . snd) cs
	where m = queryE q
queryE q e@(Var _) = q e
queryE q e@(Nil) = q e
queryE q e@(Box e1) = q e++m e1
	where m = queryE q
queryE q e@(Translate e1 e2) = q e++m e1++m e2
	where m = queryE q
queryE q e@(Colour e1 e2) = q e++m e1++m e2
	where m = queryE q
queryE q e@(HasType _ e1) = q e++m e1
	where m = queryE q


--queryE q e = error $ "queryE: unknown expr "++show e 

freeVars :: E -> [String]
freeVars e = fv [] e
    where fv e (If p c a) = fv e p ++ fv e c ++ fv e a
          fv e (Lam n bd) = fv (n:e) bd
          fv e (App lm ar) = fv e lm ++ fv e ar
          fv e (Var n) | n `elem` e = []
                       | otherwise = [n]
          fv e (Sig s) = fv e s
          fv e (SigVal s) = fv e s
          fv e (SigAt s1 s2 ) = fv e s1 ++ fv e s2
          fv e (M1 _ s) = fv e s
          fv e (M2 _ s1 s2 ) = fv e s1 ++ fv e s2
          fv e (Not s) = fv e s
          fv e (Cmp _ s1 s2) = fv e s1 ++ fv e s2
          fv e (And s1 s2) = fv e s1 ++ fv e s2
          fv e (Or s1 s2) = fv e s1 ++ fv e s2
          fv e (Cons s1 s2) = fv e s1 ++ fv e s2
          fv e (Pair s1 s2) = fv e s1 ++ fv e s2
          fv e (SigDelay s1 s2) = fv e s1++ fv e s2
          fv e (Box s1) = fv e s1
          fv e (Translate s1 s2) = fv e s1++ fv e s2
          fv e (Colour s1 s2) = fv e s1++ fv e s2
          fv e (HasType _ s2) =fv e s2
          fv e (Event s1) = fv e s1
          fv e (Const _) = []
          fv e (Nil) = []
          fv e (LetE ses er) = fv (map fst ses++e) er ++ concatMap (fv (map fst ses++e) . snd) ses 
          fv e (Case te pats) = fv e te ++ concatMap (\(pat, ep) -> fv (patIntroducedVars pat++e) ep) pats
          
          --fv e (expr) = []

mapE :: (E-> E)-> E -> E
mapE f (If p c a) = f (If (m p) (m c) (m a))
    where m = mapE f 
mapE f (Lam n bd) = f (Lam n (m bd))
    where m = mapE f
mapE f (App le ae) = f (App (m le) (m ae))
    where m = mapE f
mapE f (Var n) = f (Var n)
mapE f (Sig s) = f (Sig (mapE f s))
mapE f (SigVal s) = f (SigVal (mapE f s))
mapE f (SigDelay s1 s2) = f (SigDelay (mapE f s1) (mapE f s2))
mapE f (SigAt s1 s2) = f (SigAt (mapE f s1) (mapE f s2))
mapE f (M1 m s) = f (M1 m (mapE f s))
mapE f (M2 m s1 s2) = f (M2 m (mapE f s1) (mapE f s2))
mapE f (And s1 s2) = f (And (mapE f s1) (mapE f s2))
mapE f (Or s1 s2) = f (Or (mapE f s1) (mapE f s2))
mapE f (Cons s1 s2) = f (Cons (mapE f s1) (mapE f s2))
mapE f (Not s) = f (Not (mapE f s))
mapE f (Cmp o s1 s2) = f (Cmp o (mapE f s1) (mapE f s2))
mapE f (Pair s1 s2) = f (Pair (mapE f s1) (mapE f s2))
mapE f (Event s2) = f (Event (mapE f s2))
mapE f (Const c) = f (Const c)
mapE f (Nil) = f (Nil)
mapE f (LetE ses er) = 
    f (LetE (map (\(n,e)-> (n, mapE f e)) ses) (mapE f er))
mapE f (Switch ses er) = 
    f (Switch (map (\(e1,e2)-> (mapE f e1, mapE f e2)) ses) (mapE f er))
mapE f (Case te pats) = 
    f (Case (mapE f te) (map (\(p,e)-> (p, mapE f e)) pats))


mapE f (Box s1) = f (Box (mapE f s1))
mapE f (Translate s1 s2) = f (Translate (mapE f s1) (mapE f s2))
mapE f (Colour s1 s2) = f (Colour (mapE f s1) (mapE f s2))
mapE f (HasType t s2) = f (HasType t (mapE f s2))


--mapE f e = error $ "mapE: unknown expr "++show e 

--mapE f e = f e

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

        -- sin = M1 Sin
        -- cos = M1 Cos



data Pat = 	  PatVar String
		| PatIgnore
		| PatLit V
		| PatPair Pat Pat
		| PatNil 
		| PatCons Pat Pat
		-- | PatGuard E Pat
		deriving (Show, Eq, Read)

ppPat (PatVar n) = n
ppPat (PatIgnore ) = "_"
ppPat (PatLit e) = show e
ppPat (PatPair x y) = "("++ppPat x++","++ppPat y++")"
ppPat (PatNil) = "[]"
ppPat (PatCons x xs) = "("++ppPat x++":"++ppPat xs++")"

patIntroducedVars (PatVar nm) = [nm]
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


{-
t = Const $ BoolV True
f = Const $ BoolV False


class ToE a where
    toExpr :: a -> E

instance ToE E where
    toExpr = id

instance ToE [Char] where
    toExpr = Var

sig :: ToE a => a -> E
sig = Sig . toExpr

sigVal :: ToE a => a -> E
sigVal = SigVal . toExpr

sigDelay :: (ToE a, ToE b) => a -> b-> E
sigDelay x y = SigDelay (toExpr x) (toExpr y)

x ^$> y = (toExpr x) $> (toExpr y)

-}