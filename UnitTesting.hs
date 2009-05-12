module UnitTesting where

import EvalM
import Eval
import Expr
import HaskSyntaxUntyped
import Data.Maybe

type TError = String

type Test = IO (Maybe TError)

ok = return Nothing
failure  = return . Just  
failures = failure . concat

infix 1 `evalsTo`, `is`, `equals`
 
evalsTo :: E -> V -> Test
evalsTo = evalsToInContext []

evalsToInContext :: [(String, V)] -> E -> V -> Test
evalsToInContext env e v = case eval (extsEnv env emptyEvalS) e of
                             Error s  -> failure $ "eval failed with error "++s
                             Res vr -> if vr==v 
                                          then ok
                                          else failures [ pp e, " => ", show vr, ", expected ", show v]


is :: Show a => a -> (a->Bool) -> Test
is x px = if px x
             then ok
             else failures ["fails unknown \"is\": ", show x]

equals :: (Show a, Eq a) => a -> a -> Test
equals x y = if x==y
                then ok
                else failures ["fail: ", show x, " /= ", show y]

test_foo = 1+1 `evalsTo` 2

runTests :: [Test] -> IO ()
runTests tsts = do ress <- catMaybes `fmap` sequence tsts
                   mapM_ print ress
                   
                

tests = [ (Case (Pair 1  2) [(PatPair (PatVar "x") (PatIgnore), Var "x")]) `evalsTo` 1]

rt = runTests tests