{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Main where

import Math.Probably.JAGS
import Math.Probably.FoldingStats
--import Math.Probably.Distribution
import Math.Probably.GlobalRandoms
import Math.Probably.Sampler

import qualified Math.Probably.PDF as PDF
import Control.Monad
--import System.IO.Unsafe


epsp :: Double -> Sampler Double
epsp p = do 
  let n =100::Int
      q = 10::Double          
      noise = 2::Double
  nr <- binomial n p
  fmap sum $ sequence $ replicate nr $ gauss q noise

calcp (phi, plo, slop, offset) t = (phi-plo)/(1+exp(offset*slop-slop*t))+plo

pars = (0.8, 0.2, 0.02, 500)

quantalModel = Model
                  [ForEvery "i" 1 200 [
                    "x[i]" ~~ Norm ("mx[i]") ("sx[i]"),
                    "y[i]" ~~ Norm ("my[i]") ("sy[i]"),
                    "z[i]" ~~ Norm ("mz[i]") ("sz[i]"),
                    "sx[i]" <-- (recip $ "q"*"nrx[i]"*"noise"*"noise"/2),
                    "sy[i]" <-- (recip $ "q"*"nry[i]"*"noise"*"noise"/2),
                    "sz[i]" <-- (recip $ "q"*"nrz[i]"*"noise"*"noise"/2),
                    "mx[i]" <-- "nrx[i]"*"q",
                    "my[i]" <-- "nry[i]"*"q",
                    "mz[i]" <-- "nrz[i]"*"q",
                    "nrx[i]" ~~ Binomial "p1" "round(n)",
                    "nry[i]" ~~ Binomial "p2" "round(n)",
                    "nrz[i]" ~~ Binomial "p3" "round(n)"
                    ],
                   "noise" <-- 1/sqrt "tau",
                   "q" ~~ Uniform 5 20,
                   "p1" ~~ Beta 1 1,
                   "p2" ~~ Beta 1 1,
                   "p3" ~~ Beta 1 1,
                   "n" ~~ Uniform 50 200,
                   "tau" ~~ Gamma 0.01 0.01
                  ]

qData = [("x", sampleN 200 $  epsp 0.2),
         ("y", sampleN 200 $  epsp 0.5),
         ("z", sampleN 200 $  epsp 0.8)]

getP = words "n q p1 p2 p3 noise"


quantalModel1 = Model
                  [ForEvery "i" 1 1000 [
                    "x[i]" ~~ Norm ("m[i]") ("s[i]"),
--                    "s[i]" <-- (recip $ "q"*"nr[i]"*"noise"*"noise"/2),
                    "s[i]" <-- ("tau1"/"q"*"nr[i]"),
                    "m[i]" <-- "nr[i]"*"q",
                    "nr[i]" ~~ Binomial "p[i]" "round(n)",
                    "p[i]" <-- ("phi"-"plo")/(1+exp("offset"*"slop"-"slop"*"i"))+"plo"
                    ],
                   "cv1" <-- 1/("q"*sqrt "tau1"),
                   "q" ~~ Uniform 5 20,
                   "phi" ~~ Beta 1 1,
                   "plo" ~~ Beta 1 1,
                   "slop" ~~ Uniform 0 0.1,
                   "offset" ~~ Uniform 200 800,
                   "n" ~~ Uniform 50 400,
                   "tau1" ~~ Gamma 0.01 0.01
                  ]
quantalModel2 = Model
                  [ForEvery "i" 1 1000 [
                    "x[i]" <-- "sum()",
                    ForEvery "j" 1 400 [
                      "cur[i][j]" ~~ Norm "muij[i][j]" "tau1",
                      "muij[i][j]" <-- "qs[j]"*"step(n-j)"
                     ],
                    "p[i]" <-- ("phi"-"plo")/(1+exp("offset"*"slop"-"slop"*"i"))+"plo"
                    ],
                   ForEvery "j" 1 400 [
                    "qs[j]" ~~ Norm "q" ("tau2")
                    ],
                   "cv1" <-- 1/("q"*sqrt "tau1"),
                   "cv2" <-- 1/("q"*sqrt "tau2"),
                   "q" ~~ Uniform 5 20,
                   "phi" ~~ Beta 1 1,
                   "plo" ~~ Beta 1 1,
                   "slop" ~~ Uniform 0 0.1,
                   "offset" ~~ Uniform 200 800,
                   "n1" <-- 5,
                   "n" ~~ Uniform 50 400,
                   "tau1" ~~ Gamma 0.01 0.01,
                   "tau2" ~~ Gamma 0.01 0.01
                  ]

getP1 = words "n q phi plo slop offset cv1"
getP2 = words "n q phi plo slop offset cv1 cv2"

qData1 = [("x", head $ sampleN 1 $ mapM epsp (map (calcp pars) [0..1000]))]

subc from to = map f where
   f c | c == from = to
       | otherwise = c 

main = do
  res <- runModel quantalModel1 20000 20000 qData1 getP1
  mapM print res
  return ()
