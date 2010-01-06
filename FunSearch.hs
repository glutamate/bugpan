{-# LANGUAGE DeriveDataTypeable #-}

module FunSearch where

import Data.Generics
import Math.Probably.Sampler
import Control.Monad
import System.Cmd

data E = X | One | Two | BinOp Op E E | Exp E | Pow Int E deriving (Eq, Show, Typeable, Data)

data Op = Add | Sub | Mul | Div deriving (Eq, Show, Typeable, Data)

pp X = "x"
pp One = "1"
pp Two = "2"
pp (BinOp op e1 e2) = "("++pp e1 ++ ppOp op++ pp e2++")" 
pp (Exp e) = "exp("++pp e++")"
pp (Pow n e) = "("++pp e++")^"++show n


ppOp Add = "+"
ppOp Mul = "*"
ppOp Sub = "-"
ppOp Div = "/"


selOp 0 = Add
selOp 1 = Sub
selOp 2 = Mul
selOp 3 = Div

isConst One = True
isConst Two = True
isConst X = False
isConst (BinOp Sub X X) = True
isConst (BinOp Div X X) = True
isConst (BinOp _ e1 e2) = isConst e1 && isConst e2 
isConst (Exp e2) = isConst e2
isConst (Pow n e2) = isConst e2

reifyOp Add = (+)
reifyOp Mul = (*)
reifyOp Sub = (-)
reifyOp Div = (/)

flatten :: E -> [E]
flatten = everything (++) ([] `mkQ` (:[]))

genE :: Sampler E
genE = do x <- oneOf [(0::Int)..4]
          case x of
            0 -> return $ X
            1 -> return $ One
            2 -> return $ Two
            3 -> do op<- selOp `fmap` oneOf [0..3]
                    liftM2 (BinOp op) genE genE  
            4 -> Exp `fmap` genE
            5 -> do e <- genE
                    return $ Pow 2 e

eval :: E -> Double -> Double
eval X x = x
eval One _ = 1
eval Two _ = 2
eval (BinOp op e1 e2) x = reifyOp op (eval e1 x) (eval e2 x)
eval (Exp e) x = exp $ eval e x
eval (Pow n e) x = (eval e x)**(realToFrac n)

countExps e = length [ e' | Exp e' <- flatten e]

onlyRootExps (Exp _) = True
onlyRootExps (BinOp _ (Exp _) _) = True
onlyRootExps (BinOp _ _ (Exp _)) = True
onlyRootExps e = countExps e == 0

hasXs e = length [ X | X <- flatten e] > 0

checkComplexity e = let n = length $ flatten e
                    in n >4 && n < 10

noConstFactor (BinOp Mul e1 e2) = not $ isConst e1 || isConst e2
noConstFactor _ = True

noConstExp e = null [ e' | Exp e' <- flatten e, isConst e']

asymZero e = let vn = eval e $ -10
                 vp = eval e 10
             in abs vn <0.5 && abs vp < 0.5

accept e = countExps e == 1 && 
           onlyRootExps e && 
           hasXs e && 
           checkComplexity e && 
           noConstFactor e && noConstExp e && asymZero e
                         



main = do es <- (runSamplerSysRan $ genE)
          forM_ (take 10 $ filter accept es) $ \e-> do
            let str = "echo 'plot "++pp e++"; pause mouse key' | gnuplot"
            putStrLn str
            system $ "echo 'plot "++pp e++"; pause mouse key' | gnuplot"
            return ()