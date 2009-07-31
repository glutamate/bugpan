module PrettyPrint where

import EvalM
import Expr
import Numbers
import Data.List 

ppType :: T -> String
ppType BoolT = "Bool"
ppType UnitT = "()"
ppType (PairT t1 t2) = "("++ppType t1++","++ppType t2++")"
ppType (LamT t1 t2) = "("++ppType t1++")-> ("++ppType t2++")"
ppType (ListT t1) = "["++ppType t1++"]"
ppType (TyVar s) = s
ppType (NumT Nothing) = "Number"
ppType (NumT (Just IntT)) = "Integer"
ppType (NumT (Just RealT)) = "Real"
ppType (NumT (Just CmplxT)) = "Complex"
ppType (SignalT t) = "Signal "++ppType t
ppType t = show t

ppVal (BoolV True) = "True"
ppVal (BoolV False) = "False"
ppVal (NumV v) = ppNum v
ppVal (LamV _) = "<lambda value>"
ppVal (SigV t1 t2 dt _) = "<signal value "++show t1++" to  "++show t2++">"
ppVal (PairV (PairV x y) z) = "("++ppVal x++","++ppVal y++","++ppVal z++")"
ppVal (PairV v w) = "("++ppVal v++","++ppVal w++")"
ppVal (Unit) = "()"
ppVal (ListV vs) =  "["++slist vs++"]"
        where slist [] = ""
              slist (x:[]) = ppVal x
              slist (x:xs) = ppVal x ++ ", " ++ slist xs
ppVal (BoxV shp loc col) = "Box " ++ppVal shp++" @"++ppVal loc++" RGB"++ppVal col
ppVal (StringV s) = show s

ppProg :: String -> [Declare]->String
ppProg modNm ds= "module "++modNm++" where\n"++(unlines $ map ppDecl ds)

ppDecl (Let nm e ) = nm++" = " ++ pp e
ppDecl (SinkConnect e (src,arg)) = (pp e++" *> " ++ src++ " "++pp arg)
ppDecl (ReadSource varNm (src, arg)) = (varNm++" <* " ++ src ++ " "++pp arg)
ppDecl (Import nm []) = "use "++nm 
ppDecl (Import nm substs) = "use "++nm++" { "++intercalate "," (map ppImpSubst substs) ++ " }"
    where ppImpSubst (nm,e) = nm++" => "++pp e
ppDecl (DeclareType nm t) = nm ++" :: "++ ppType t
ppDecl (Stage nm i) = "stage "++nm++" "++show i
ppDecl (Nop) = "Nop"

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
pp (Lam n e) = concat ["\\", n, "->(", pp e, ")"]
pp (Var n) = n
pp (Const v) = ppVal v
pp (App f a) = ppa f ++ " " ++ ppa a
--pp (Pair (Pair x y) z) = concat ["(", pp x , ", ", pp y,", ", pp z, ")"]
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
pp (M1 Exp e) = "exp " ++ ppa e
pp (M1 Ln e) = "log " ++ ppa e
pp (LetE les efinal) = concat ["let {", intercalate ";" $ ppes les, "} in ", ppa efinal]
    where ppes es = map (\(n,t,e)-> n++" = "++pp e) es 
pp (Box d) = "box "++ppa d
pp (Translate t e) = "translate "++ppa t++" "++ppa e
pp (Colour t e) = "colour "++ppa t++" "++ppa e
pp (Case tst pats) = "case "++pp tst++" of {"++ (intercalate ";" $ map (\(pat,e)-> ppPat pat++" -> "++pp e) pats)++"}"
pp e = show e

pp2op e1 op e2 = ppa e1 ++ op ++ ppa e2

ppPat (PatVar n) = n
ppPat (PatIgnore ) = "_"
ppPat (PatLit e) = show e
ppPat (PatPair x y) = "("++ppPat x++","++ppPat y++")"
ppPat (PatNil) = "[]"
ppPat (PatCons x xs) = "("++ppPat x++":"++ppPat xs++")"

