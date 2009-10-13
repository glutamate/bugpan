module BugAnal where

import Text.Regex.Posix

mkAnalyser :: Int -> [String] -> [String]
mkAnalyser n (('>':q):ss) = 
    let (ndiff, str) = procQ n q
    in map (indent n) str++mkAnalyser (n+ndiff)  ss
mkAnalyser _ [] = []
mkAnalyser n ss = 
    let (para, rest) = span justText ss
    in (map ((indent n) . 
             ("io $ putStrLn "++) . 
             show) (("<p>":para)++["</p>"])) ++mkAnalyser n rest


justText ('>':s) = False
justText s = True

indent n = ((replicate n ' ')++)

--todo:
-- running it
-- include all variables when opening session
-- query results
-- plots!

--later:
-- filter
-- allSessions
-- what about text in many-sessions?
-- tables for many-sessions
-- >> to include query in output
-- running goals ?

procQ n s 
    | s =~ "^\\s*(\\w+)\\s*@=(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*@=(.+)")::[[String]]     
           in (0, ["let "++lhs ++" = "++rhs,"storeAs "++show lhs ++" "++lhs])
    | s =~ "^\\s*(\\w+)\\s*=(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*=(.+)")::[[String]]     
           in (0, ["let "++lhs ++" = "++rhs])
    | s =~ "^\\s*inSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*inSession\\s*(.+)")::[[String]]
           in (3, ["inApproxSession "++show sessnm++" $ do"])
    | s =~ "^\\s*close\\s*" = (-3, [])
    | s =~ "^\\s*openSession\\s*" = 
           (3, ["inSessionFromArgs $ do"])
    | otherwise = (0, ["unknown"])


tst =  ["hello world",
        "> openSession",
        "> foo @= bar + zoo",
        "how are you",
        "> x = y",
        "> close",
        "foobar"]

main = putStrLn $ unlines $ mkAnalyser 3 tst
