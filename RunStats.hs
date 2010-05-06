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
ppSubParStype nm rvs = "data "++nm++" = "++nm++" {\n"++(intercalate ",\n" $ catMaybes $ map pprv rvs)++"}\n\n"
    where ind = replicate 4 ' '
          pprv (RVar vnm t _ ) | hasExclaim vnm = Nothing
                               |otherwise = Just $ ind ++vnm++" :: Param "++haskTypeString t
          pprv (InEvery dnm rs) = Just $ ind ++ dnm++" :: [Par"++dnm++"]\n"

ppUpdaters :: [String] -> [RVar]  -> [RVar] -> String
ppUpdaters path allrvs rs@((RVar vnm t ex):rest) = 
    unlines ["update"++vnm++" thedata allpars = do",
             ind++"new"++vnm++" <- metSampleP "++show vnm++" (\\"++vnm++" -> ",
             intercalate "+\n" $ map (((ind++ind)++) . pp . distE) dists,
             ind++ind++") $ "++vnm++" " ++intercalate "$" path ++" allpars",
             "-- children: "++show (childrenOf vnm allrvs)
            ] -- full path
    where ind = replicate 4 ' '
          childDist cnm = ($>(Var (last cnm))) $ lookupDist allrvs $ last cnm
          dists = map (unPex [vnm] allrvs) $ ex : (map childDist $ childrenOf vnm allrvs)

distE (App (Var "unknown") _) = 1
distE (App (App (Var "N") mue) sde) = Var "P.logGaussD" $> mue $> sde
distE (App (App (App (Var "N") mue) sde) xe) = Var "P.logGaussD" $> mue $> sde $> xe

lookupDist :: [RVar] -> String -> E
lookupDist rvars nm = head $ [e | RVar nm' t e <- flattenRVars rvars, nm' ==nm]

childrenOf :: String -> [RVar] -> [[String]]
childrenOf nm rvs = 
    [[nm'] | RVar nm' t e <- rvs, Var nm `elem` flatE e]++ -- first attempt
    concat [map (dnm:) $ childrenOf nm moreRvars | InEvery dnm moreRvars <- rvs]
unPex notThese rvs = mapE f
        where f (Var nm) | nm `elem` [vnm | RVar vnm _ _ <- flattenRVars rvs] &&
                           not (nm `elem` notThese) 
                             = (App (Var "unP") (Var nm))
                         | otherwise = Var nm
              f e = e


{-

plan :

-ML estimator
-each updater
-load data

-}