module BugAnal where

import Text.Regex.Posix

mkAnalyser :: Int -> [String] -> [String]
mkAnalyser n (('>':q):ss) = 
    let (ndiff, str) = procQ n q
    in str++mkAnalyser (n+ndiff)  ss
mkAnalyser _ [] = []
mkAnalyser n ss = 
    let (para, rest) = span justText ss
    in (map ((indent n) . 
             ("io $ putStrLn "++) . 
             show) ("<p>":para)++["</p>"]) ++mkAnalyser n rest


justText ('>':s) = False
justText s = True

indent n = ((replicate n ' ')++)

procQ n s | s =~ "" = (0, ["assignment"])
          | otherwise = (0, ["unknown"])


tst =  ["hello world",
        "how are you",
        "> x = y",
        "foobar"]

tsta = mkAnalyser 0 tst