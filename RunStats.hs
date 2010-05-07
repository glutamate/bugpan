module Main where

import Parse
import Expr
import System.Environment
import Database
import Traverse
import EvalM as EM
import Eval
import Numbers 
import Control.Monad
import Control.Monad.State.Lazy
import TNUtils
import Math.Probably.Sampler
import Math.Probably.Distribution
import Query (bugpanRootDir)
import ReactiveDistributions
import Query
import QueryTypes
import Data.List
import Data.Maybe
import Stats.Simulate
import Types
import PrettyPrint
--ideas:
-- exclamation mark for saving values in simfakes 

help = putStrLn $ unlines [
        "runstats simfakes {model file} {session base} {session count}\n\n",
        "runstats estimate {model file} {session filter}\n\n"
        ]       

main = getArgs >>= dispatch

unConst (Const e) = e 

dispatch ("simfakes":filenm:sessBase:_) = do
  ds <- fileDecls filenm []
  --mapM print ds
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  fakeenv <- fmap (EM.env . head) $ runSamplerIO $ evalSim es ds
  --mapM print $ fakeenv
  saveEnv sessBase tmax dt $ fakeenv
  return ()

dispatch ("estimate":filenm:sessFiltr:_) = do
  ds <- fileDecls filenm []
  let tmax = unsafeReify $ unConst $ fromJust $ lookup "_tmax" $ declsToEnv ds
  let dt = unsafeReify $ unConst $ fromJust $ lookup "_dt" $ declsToEnv ds
  let es = EvalS dt tmax Nothing []
  let rvs = rvars ds
  putStrLn $ ppParamTypes rvs

  putStrLn $ ppUpdaters [] rvs rvs
  return ()


dispatch _ = help

data RVar = RVar String T E
          | InEvery String [RVar]

flattenRVars :: [RVar] -> [RVar]
flattenRVars [] = []
flattenRVars (r@(RVar _ _ _):rs) = r : flattenRVars rs
flattenRVars (r@(InEvery _ subs):rs) = r : (flattenRVars subs ++ flattenRVars rs)

rvars :: [Declare] -> [RVar]
rvars = concatMap rvar 
   where rvar (Distribute p e) = [RVar (unsafePatToNameWithBang p) (typeOfDist e) e]
         rvar (Every p dure decls) = [InEvery (durExprToName dure) $ concatMap rvar decls]
         rvar _ = []

typeOfDist (App (App (Var "N") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "uniform") _) _) = NumT (Just RealT)
typeOfDist (App (App (Var "RandomSignal") _) _) = SignalT $ NumT (Just RealT)
typeOfDist (App (Var "unknown") _) = NumT (Just RealT)
typeOfDist e = error $ "typeOfDist: unknown expr" ++ show e

durExprToName (Var nm) = nm
durExprToName (App (Var nm) _) = nm

ppParamTypes :: [RVar] -> String
ppParamTypes rs = 
    let topPars = [ppSubParStype ("Par"++nm) rvs | InEvery nm rvs <- flattenRVars rs]
    in ppSubParStype "AllPars" rs++concat topPars

ppSubParStype :: String -> [RVar] -> String
ppSubParStype nm rvs = "data "++nm++" = "++nm++" {\n"++(intercalate ",\n" $ map pprv rvs)++"}\n\n"
    where ind = replicate 4 ' '
          pprv (RVar vnm t _ ) | hasExclaim vnm =  ind ++noExclaim vnm++" :: "++haskTypeString t
                               | otherwise =  ind ++vnm++" :: Param "++haskTypeString t
          pprv (InEvery dnm rs) =  ind ++ dnm++" :: [Par"++dnm++"]\n"

ppUpdaters :: [String] -> [RVar]  -> [RVar] -> String
ppUpdaters _ _ [] = ""
ppUpdaters outerPath allrvs rs@((InEvery durnm rvs):rest) = 
    ppUpdaters (durnm:outerPath) allrvs rvs ++ ppUpdaters outerPath allrvs rest
ppUpdaters outerPath allrvs ((RVar vnm t ex):rest) = 
    if (hasExclaim vnm) then "" else alllines 
    where alllines = unlines [updName++" allpars = do",
             ind++"new"++vnm++" <- "++smany++" metSampleP "++show vnm++" (\\"++vnm++" -> ",
             intercalate "+\n" $ map (((ind++ind)++) . pp) dists,
             ind++ind++") $ "++parpath, -- vnm++" $ " ++concatMap (++" $") outerPath ++" allpars",
             ind++"return $ "++retval++"\n" -- children: "++show (childrenOf vnm allrvs)++"\n"
            ] ++ ppUpdaters outerPath allrvs rest
          updName = "update_"++(concat $ map (++"_") outerPath)++vnm
          smany = case outerPath of
                    [] -> "" -- forIdx2' (session allpars) running
                    [p] -> "sampleMany $ forIdx ("++p++" allpars) $ \\"++p++"-> "
                    [p1,p2] -> "sampleMany2 $ forIdx' (" ++p2++" allpars) "++p1++" $ \\"++p2++" "++p1++"-> "
          retval = case outerPath of
                    [] -> "allpars { "++vnm++" = new"++vnm++" }"
                    [p] -> "allpars { "++p++" = map ("++p++" allpars) $ \\r -> r { "++vnm++" = new"++vnm++" } }"
                    [p2, p1] -> "allpars { "++p1++" = map ("++p1++" allpars) $ \\r1 -> r1 { "++p2++" = map ("++p2++" r1) $ \\r2 -> r2 { "++vnm ++" = new"++vnm++" } }"
          ind = replicate 4 ' '
          ppath myout cnm = (pathE $ pathOf myout cnm allrvs)
          distSum myout [cnm] = unPex myout [vnm] allrvs $ distE $ childDist cnm
          distSum myout (cnm:cnms) = Var "sum" $> 
                                     ((Var "for" $> ppath myout cnm) $> 
                                     Lam cnm UnspecifiedT (distSum (cnm:myout) cnms))
          childDist cnm = ($>(Var cnm)) $ lookupDist allrvs cnm
          lh = distE (ex $> Var vnm)
          parpath = pp $ pathE $ pathOf outerPath vnm allrvs
          dists = unPex outerPath [vnm] allrvs lh : (map (distSum outerPath) $ childrenOf vnm allrvs)

forIdx2' :: [a] -> (a->[b]) -> (a -> b -> c) -> [[c]]
forIdx2' xs f g = map (\x-> map (g x) $ f x) xs

distE (App (App (Var "unknown") _) _)= 1
--distE (App (App (Var "N") mue) sde) = Var "P.logGaussD" $> mue $> sde
distE (App (App (App (Var "N") mue) sde) xe) = Var "P.logGaussD" $> mue $> sde $> xe
distE d = error $ "distE: "++show d
lookupDist :: [RVar] -> String -> E
lookupDist rvars nm = head $ [e | RVar nm' t e <- flattenRVars rvars, nm' ==nm]

pathE :: [String] -> E
pathE = pathE' . reverse
        where pathE' [x] = Var x 
              pathE' (x:xs) = Var x $> (pathE' xs)
              pathE' [] = Nil


pathOf :: [String] -> String -> [RVar] -> [String]
pathOf outerPath nm rvs = case pathOf' nm rvs of
                            (x:_) -> blockOuter outerPath $ "allPars":x
                            [] -> error $ "can't find path to "++nm
    where --blockOuter [] fullPath = [] -- fullPath
          blockOuter outer full =  if any (`elem` outer) full
                                      then [head outer] ++ (reverse $ takeWhile (not . (`elem` outer)) $ reverse full) 
                                      else full

pathOf' :: String -> [RVar] -> [[String]]
pathOf' nm rvs = [[noExclaim nm'] | RVar nm' t e <- rvs, nm==nm']++ -- first attempt
                 [[dnm] | InEvery dnm moreRvars <- rvs, dnm == nm]++
                 concat [map (dnm:) $ pathOf' nm moreRvars | InEvery dnm moreRvars <- rvs]

last2 [] = []
last2 [x] = [x]
last2 [x,y] = [x,y]
last2 (x:xs) = last2 xs

childrenOf :: String -> [RVar] -> [[String]]
childrenOf nm rvs = 
    [[nm'] | RVar nm' t e <- rvs, Var nm `elem` flatE e]++ -- first attempt
    concat [map (dnm:) $ childrenOf nm moreRvars | InEvery dnm moreRvars <- rvs]
unPex outer notThese rvs = mapE f
        where f (Var nm) | hasExclaim nm = pathE $ pathOf outer nm rvs
                         | nm `elem` [vnm | RVar vnm _ _ <- flattenRVars rvs] &&
                           not (nm `elem` notThese) 
                             = (App (Var "unP") $ pathE $ pathOf outer nm rvs)
                         | otherwise = Var nm
              f e = e


{-

plan :

-ML estimator
-each updater
-load data

-}