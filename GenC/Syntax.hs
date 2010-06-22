module Syntax where

import Expr
import EvalM
import PrettyPrint
import Data.List
import Numbers
import TNUtils

space = " "
semicolon = ";"

inPar s = "("++s++")"

ind s = "   "++s

data CType = CIntT
           | CLongT
           | CDoubleT
           | CArrayT (Maybe E) CType
           | CPtrT CType
           | CCharT
           | VoidT
           | CStringT
           | CStructT String
           | CAnyTy String
           deriving (Show, Eq)

bugTyToCTy (NumT (Just RealT)) = CDoubleT
bugTyToCTy (ListT (PairT (NumT (Just RealT)) UnitT)) = CPtrT $ CStructT "event_unit"
bugTyToCTy (SignalT t) = bugTyToCTy t
bugTyToCTy (EventT t) = CPtrT $ CStructT "event_unit"
bugTyToCTy (StringT) = CStringT

bugTyToCTy t = error2 "bugTyToCTy: "  t

data TopDecl = CInclude Bool String
             | CDefine String E
             | DeclareGlobal CType String (Maybe E)
             | CFun CType String [(String, CType)] [CCmd]
           deriving (Show, Eq)


data CCmd = DecVar CType String (Maybe E)
          | Assign E E
          | CComment String
          | IfC E [CCmd] [CCmd]
          | Return E
          | For CCmd E CCmd [CCmd]
          | Call String [E]
          | LitCmds [String]
           deriving (Show, Eq)

forCount nm from to = For (Assign (Var nm) from) (Cmp Lt (Var nm) to) (Assign (Var nm) (Var nm+1))

isInclude (CInclude _ _) = True
isInclude _ = False

ppCProg :: [TopDecl] -> String
ppCProg prg = 
        let (incls, rest) = partition isInclude prg
        in unlines $ concatMap ppCTop incls++concatMap ppCTop rest
        
ppCTop (CInclude True nm) = ["#include <"++nm++">"]
ppCTop (CInclude False nm) = ["#include \""++nm++"\""]
ppCTop (CDefine nm e) = [concat ["#define ",nm,ppEC e]]
ppCTop (DeclareGlobal ty nm Nothing) = cline [ppCTy ty, space, nm] 
ppCTop (DeclareGlobal ty nm (Just e)) = cline [ppCTy ty, space, nm, " = ", ppEC e] 
ppCTop (CFun ty nm args cmds) = 
       [concat [ppCTy ty, space, nm, inPar (intercalate "," (map parg args)), " {"]]++
       concat(map (map ind . ppCCmd) cmds)++["}"]

parg (nm, ty) = ppCTy ty ++" "++nm

ppCTy CIntT = "int"
ppCTy CLongT = "long"
ppCTy CDoubleT = "double"
ppCTy CCharT = "char"
ppCTy (CAnyTy nm) = nm
ppCTy VoidT = "void" 

ppCTy CStringT = "char *" 
ppCTy (CStructT nm) = "struct "++nm 
ppCTy (CArrayT (Just ne) t) = ppCTy t++"["++ppEC ne++"]"
ppCTy (CArrayT (Nothing) t) = ppCTy t++"[]"
ppCTy (CPtrT t) = ppCTy t ++"*"


cline :: [String]-> [String]
cline strs = [concat $ strs++[semicolon]]

ppCCmd :: CCmd -> [String]
ppCCmd (DecVar ty nm (Nothing)) = cline [ppCTy ty, space, nm] 
ppCCmd (DecVar ty nm (Just e)) = cline [ppCTy ty, space, nm, " = ", ppEC e] 
ppCCmd (Assign lhs@(Var nm) rhs@(M2 Add (Var nm') (Const (NumV (NInt 1))))) 
       | nm == nm' = [nm++"++;"]
       | otherwise = cline [ppEC lhs, " = ", ppEC rhs]
ppCCmd (Assign lhs rhs) = cline [ppEC lhs, " = ", ppEC rhs]
ppCCmd (Return e) = cline ["return (", ppEC e,")"]
ppCCmd (Call fnm args) = cline $ [printCall ((Var fnm):args)]
ppCCmd (IfC pe ccmds []) = 
       [concat ["if(", ppEC pe, ") {"]]++concat(map (map ind . ppCCmd) ccmds)++["}"]
ppCCmd (IfC pe ccmds acmds) = 
       [concat ["if(", ppEC pe, ") {"]]++concat(map (map ind . ppCCmd) ccmds)++
       ["} else {"]++concat(map (map ind . ppCCmd) acmds)++["}"]
ppCCmd (For ini tst incr cmds) = 
       [concat ["for(", head $ ppCCmd ini, 
                ppEC tst, semicolon,init $ head $ ppCCmd incr, ") {"]]
       ++concat(map (map ind . ppCCmd) cmds)++["}"]
ppCCmd (LitCmds ss) = ss
ppCCmd (CComment s) = ["//"++s]
                                      
ppEC (If p c a) = "("++ppEC p ++")?("++ppEC c ++"):("++ppEC a++")"
ppEC (App (Var "sizeof") e2) = "sizeof("++ppEC e2++")"
ppEC (App (App (Var "ptr") (Var pnm)) (Var field)) = pnm++"->"++field
ppEC (App (App (Var "dot") (Var pnm)) (Var field)) = pnm++"."++field
ppEC a@(App e1 e2) = printCall $ flatApp a
ppEC (Const x) = ppVal x
ppEC (M1 Exp e1) = "exp("++ppEC e1++")"
ppEC (M1 Ln e1) = "log("++ppEC e1++")"
ppEC (M2 Mul e1 e2) = inPar $ ppOp e1 "*" e2
ppEC (M2 Add e1 e2) = inPar $ ppOp e1 "+" e2
ppEC (M2 Sub (Const (NumV (NInt 0))) e2) = "-" ++ppEC e2
ppEC (M2 Sub e1 e2) = inPar $ ppOp e1 "-" e2
ppEC (M2 Div e1 e2) = inPar $ ppOp e1 "/" e2
ppEC (Cmp Lt e1 e2) = inPar $ ppEC e1 ++ " < " ++ ppEC e2
ppEC (Cmp Gt e1 e2) = inPar $ ppEC e1 ++ " > " ++ ppEC e2

ppEC (Var nm) = nm
ppEC (SigVal (Var nm)) = nm++"Val"
ppEC (And e1 e2) = inPar $ ppEC e1++"&&"++ppEC e2 
ppEC e = error2 "ppEC: " e

ppOp e op e2 = ppEC e++op++ppEC e2

printCall ((Var fnm):args) = fnm++"("++intercalate "," (map ppEC args)++")"

flatApp (App e1 e2) = flatApp e1 ++ flatApp e2
flatApp e = [e]

