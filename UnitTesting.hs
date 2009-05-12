{-# LANGUAGE FlexibleInstances #-}

module UnitTesting where

import EvalM
import Eval
import Expr
import HaskSyntaxUntyped
import Data.Maybe
import Text.Regex.Posix
import System.Directory
import Control.Monad
import System.Process 
import System.Exit
import System.IO

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

isTrue b =  if b
                then ok
                else failures ["fail: not true"]

test_foo = 1+1 `evalsTo` 2

class ToTests a where
    toTests :: a->[Test]
            
instance ToTests (IO (Maybe String)) where
    toTests x = [x]

instance ToTests [IO (Maybe String)] where
    toTests x = x

runTest :: ToTests a => String -> a -> IO ()
runTest s tsts = do 
  ress <- catMaybes `fmap` sequence (toTests tsts)
  mapM_ (putStrLn . (s++)) ress
                   
                

tests = [ (Case (Pair 1  2) [(PatPair (PatVar "x") (PatIgnore), Var "x")]) `evalsTo` 1]

rt = runTest "" tests

isHsFile :: String -> Bool
isHsFile = (=~ "^[A-Z].*[.]hs$")

isTestLine = (=~ "^test_[a-z1-9A-Z_]*\\s=")

r1 = "test_foo1_bar" =~ "test_[a-z1-9A-Z_]*" :: String
-- r2 =


dropRet = filter (not . (`elem` "\n\r"))

allHsFiles :: IO [String]
allHsFiles = filter isHsFile `fmap` (getDirectoryContents =<< getCurrentDirectory)

runAllTests  = do
  files <- allHsFiles
  {-concat `fmap`-}
  forM_ files $ \fn -> do
                   tsts <- (map (takeWhile (not . (`elem` " ="))) . filter isTestLine .  lines) `fmap` readFile fn
                   forM_ tsts $ \tst-> do
                         let cmd = concat ["ghc ", fn, " -e 'runTest \"\" "++tst++"'"]
                         out <- sh cmd
                         case dropRet out of
                           "" -> return ()
                           s -> putStrLn $ concat $ [fn, " ", tst, ": ", s]

sh :: String -> IO String
sh cmd = do (hin, hout, herr, ph) <- runInteractiveCommand cmd
            excode <-  waitForProcess ph
            sout <-  hGetContents hout
            serr <- hGetContents herr
            case excode of
                  ExitSuccess -> return sout
                  ExitFailure n ->
                      return $ concat ["process error ",
                                           show n,
                                           " :",
                                           serr
                                          ]