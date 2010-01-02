module Main where

import Text.Regex.Posix
import Database
import TNUtils
import Query
import QueryMaker
import QueryTypes
import EvalM
import System.Environment
import System.Cmd
import System.Exit
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe
 
--tables  



tellPrint s = tell $ "io $ putStrLn $ "++show s
tellPrintNoLn s = tell $ "io $ putStr $ "++show s

tellPrintCode s = do
  tellBeginCode
  tellPrint s -- $ "io $ putStrLn $ \""++show s++"++\""
  tellEndCode
tellBeginCode = tell $ "io $ putStrLn $ \"\\\\begin{code}\\n\""
tellEndCode = tell $ "io $ putStrLn $ \"\\\\end{code}\\n\""

tellPrintSessionName = do
    tell "sessionIdentifier <- getSessionName"
    tellBeginCode
    tell "io $ putStrLn $ \"openSession \"++sessionIdentifier"
    tellEndCode
tellEverywheres = do
  dfns <- snd `fmap` get
  --io $ print dfns
  mapM_ (tell . ("let "++)) $ reverse dfns 

modimport :: Monad m => String -> CodeWriterT m () 
modimport s = tell $ "import "++s

chomp :: String -> String
chomp s = dropWhile (==' ') s


mkAnal :: [String] -> CodeWriterT IO ()
mkAnal [] = return ()
mkAnal (('%':_):ss) = mkAnal ss
mkAnal ((">"):ss) = mkAnal ss
mkAnal (('>':q):ss) = 
    let writeQ = head q /= '>'
        q1 = if writeQ then q else tail q
        ind = indentOf q1
        (tablines, rest) = span (\ln-> not (justText ln) && 
                                                   indentMoreThan ind (tail ln)) ss
    in cond [("table" `isPrefixOf` (chomp q1), do
                procTable (words q1) tablines
                mkAnal rest), 
             ("t-test" `isPrefixOf` (chomp q1), do
                procTtest (words q1) tablines
                mkAnal rest),
             ("defineSession" `isPrefixOf` (chomp q1), do
                liftIO $ case words q1 of
                  defS:sessNm:goal:rest -> let ntimes = (safeHead rest >>= safeRead) `orJust` 1 in
                                           case last defS of
                                             '!' -> do
                                               s <- loadApproxSession sessNm root
                                               deleteSession s
                                               runGoal ("new:"++sessNm) goal ntimes
                                             '+' -> ifM (existsSession sessNm root)
                                                        (runGoal sessNm goal ntimes)
                                                        (runGoal ("new:"++sessNm) goal ntimes)
                                             _ -> whenM (not `fmap` existsSession sessNm root) $
                                                         runGoal ("new:"++sessNm) goal ntimes
                  _ -> return ()
                return () )] $  do 
                     procQ writeQ $ unlines $ chomp q1 : map (tail) tablines
                     mkAnal rest

mkAnal ("":ss) = mkAnal ss
mkAnal ss = do
  let (para, rest) = span (\ln -> justText ln && (not $ null $ ln) && (not $ '%' == head ln)) ss
  tellPrint ""
  mapM tellPrint para
  tellPrint ""
  mkAnal rest 
  

runGoal :: String -> String -> Int -> IO ()
runGoal sessNm goal n = do
  system $ "ghc -O2 --make "++goal
  forM [1..n] $ const $ system $ "./"++goal++" "++sessNm
  return ()
           

justText ('>':s) = False
--justText ("") = False
justText s = True

indentOf = length . takeWhile (==' ')

indentMoreThan n = (>n) . indentOf

--todo:
-- stats?
--later:
-- filter
-- >> to not include query in output
-- running goals ?

--root = "/var/bugpan/sessions/"


allNmsTypes = do
  sesns <- getSessionInRootDir root
  getNamesAndTypes sesns

tellNmsTys = do
  nmtys <- liftIO $ allNmsTypes 
  forM_ nmtys $ \(nm,ty) -> do
               if ty == SignalT (NumT (Just RealT))
                  then tell $ unCap nm ++ " <- signalsDirect \""++ nm++"\";"
                  else tell $ concat [unCap nm, " <- "++typeToKind ty,
                                                       " \""++ nm++"\" ",
                                                       (typeToProxyName $ unWrapT ty), ";"
                                                      ]
  tellEverywheres

procTtest qs [tl1, tl2] = do
  let mfiltr = case qs of
                 "table":"where":f:_ -> Just f
                 _ -> Nothing
  tell "tres <- ttest $ do"
  let q1= (chomp $ tail $ tl1)
  let q2= (chomp $ tail $ tl2)
  indent 3
  tellNmsTys
  tell $ "return (snd $ head $"++q1++", snd $ head $ "++q2++")"
  indent $ -3
  --tellPrint "<pre>"
  tellPrintNoLn $ "<pre>> t-test '"++q1++"', '"++q2++"'\n   p < "
  tell $ "io $ putStrLn $ tres"
  tellPrint "</pre>"

  return ()

procTtest qs [tl] = do
  let mfiltr = case qs of
                 "table":"where":f:_ -> Just f
                 _ -> Nothing
  tell "tres <- ttest1 $ do"
  let q1= (chomp $ tail $ tl)
  indent 3
  tellNmsTys
  tell $ "return (snd $ head $"++q1++")"
  indent $ -3
  --tellPrint "<pre>"
  tellPrintNoLn $ "<pre>> t-test '"++q1++"'\n   p < "
  tell $ "io $ putStrLn $ tres"
  tellPrint "</pre>"

  return ()

procTable qs tablines = do
  let mfiltr = case qs of
                 "table":"where":f:_ -> Just f
                 _ -> Nothing
  tellPrintNoLn "\\begin{tabular}{ l"
  tellPrintNoLn $ concatMap (\l-> " | c") tablines
  tellPrint " }"
  tellPrintNoLn "session"
  tellPrintNoLn $ concatMap (\l-> " & "++tail (chomp l)) tablines
  tellPrint " \\"
  tellPrint "\\hline"
  tell "inEverySession_ $ do"
  indent 3
  tellNmsTys
  when (isJust mfiltr) $ do tell $ "when ("++fromJust mfiltr++") $ do"
                            indent 3                       
  --tellPrint "<tr>"
  tell "sessionIdentifier <- getSessionName"
  tell "io $ putStrLn $ take 6 sessionIdentifier"
  forM_ (tablines ) $ \ln -> do
                         tellPrintNoLn " & "
                         tell $ "askForLiterateTable $ "++chomp (tail ln)
  tellPrint " \\"
  -- tell $ "io $ putStrLn $ "
  if (isJust mfiltr) 
     then indent $ -6
     else indent $ -3
  tellPrint "\\hline"
  tellPrint "\\end{tabular}"

  return ()



procQ writeQ s 
    | s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 tell $ "storeAsOvwrt "++show lhs ++" "++lhs
                 when writeQ $ tellPrintCode $ lhs ++ " = " ++ rhs
    | s =~ "^\\s*(\\w+)\\s*=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 when writeQ $ tellPrintCode $ lhs ++ " = " ++ rhs
    | s =~ "^\\s*openSession\\s*$" = 
           do tell "inSessionFromArgs $ do"
              indent 3
              tell "setForLiterate"
              tellNmsTys
              tellPrintSessionName
    | s =~ "^\\s*inEverySession\\s*$" = 
           do tell "inEverySession_ $ do"
              indent 3
              tellNmsTys
              tellPrintSessionName
    | s =~ "^\\s*inSessionsWhere\\s*(.+)" = 
           let [[all, filtr]] = (s =~ "^\\s*inSessionsWhere\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show filtr++" $ do"
                 indent 3
                 tellNmsTys              
                 tellPrintCode $ "inSessionsWhere "++filtr
                 tell $ "when ("++filtr++") $ do"
                 indent 3
    | s =~ "^\\s*openSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*openSession\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show sessnm++" $ do"
                 indent 3
                 tellNmsTys              
                 tellPrintCode $ "openSession "++sessnm
    | s =~ "^\\s*importHs\\s*(.+)" = 
           let [[all, modnm]] = (s =~ "^\\s*importHs\\s*(.+)")::[[String]]
           in modimport modnm
    | s =~ "^\\s*plotSize\\s*(.+)x(.+)" = 
           let [[all, h, w]] = (s =~ "^\\s*plotSize\\s*(.+)x(.+)")::[[String]]
           in tell $ "plotSize "++h++" "++w
    | s =~ "^\\s*close\\s*" = do tell "return ()"
                                 indentAbs $ 5
    | s =~ "^\\s*break\\s*" = tellPrint "<div style=\"page-break-before: always\" />"
                                
    | s =~ "^\\s*everywhere\\s*(.+)" = 
           let [[_, defn]] = s =~ "^\\s*everywhere\\s*(.+)"
           in do --io $ print s
                 modify $ \(i, ss) -> (i, defn:ss)
                 --io $ putStrLn $ "remembering "++defn
                 when writeQ $tellPrintCode $ "everywhere "++defn
    | chomp s == "" = return ()
    | otherwise = do when writeQ $ do
                       tellBeginCode
                       tellPrint s 
                       tellEndCode
                       tellPrintNoLn "| => |"
                     tell $ "askForLiterate $ "++(concat $ map chomp $ lines s)
                     


initHtml = 
           {- ["<html>",
            "<head>",
             "<style>",
             "table { border-top: 1px solid black; ",
             "        border-bottom: 1px solid black;  }",
             "thead th { border-bottom: 1px solid black; }",
             --"tbody  tr { border-top: 1px solid black; }",
             --"table td, thead th { cell-spacing: 5px; }",
             "</style>",
            "</head><body>"] -}
    ["\\documentclass[a4paper]{article}",
     "%include polycode.fmt",
     "\\begin{document}"]

endHtml = 
      ["\\end{document}"]



writer s = do
  modimport "Database"
  modimport "QueryTypes"
  modimport "QueryUtils"
  modimport "Query"
  modimport "QueryPlots"
  modimport "PlotGnuplot"
  modimport "Data.List"
  modimport "TNUtils"
  tell "main = do"
  indent 3
  when (('\\'/=) $ head $ head $ s) $
        mapM_ tellPrint initHtml
  mkAnal s
  when (('\\'/=) $ head $ head $ s) $
        mapM_ tellPrint endHtml
  tell "return ()"
main = do 
  allArgs <- getArgs -- (fileNm:rest)
  let (opts, fileNm:rest) = partition beginsWithHyphen allArgs
  file <- lines `fmap` readFile fileNm
  let fileProper = head $ splitBy '.' fileNm
  let hsFile = fileProper++".hs"
  let lhsFile = fileProper++".lhs"
  code <- execCodeWriterT "Main" (writer file) 
  writeFile hsFile code
  ghcres <- if ("-p" `elem` opts)
               then system $ "ghc -O2 -prof -auto-all --make "++hsFile
               else system $ "ghc -O2 --make "++hsFile
  let profOpts =  if ("-p" `elem` opts)
                    then " +RTS -p"
                    else ""
  case ghcres of
    ExitFailure _ -> fail $ "ghc fail: "++show ghcres
    ExitSuccess -> do 
                        system $ "./"++fileProper++" "++intercalate " " ((opts\\["-o", "-a"])++rest) ++" >"++lhsFile++profOpts
                        unless ("-nopdf" `elem` opts) $ do
                             system $ "lhs2tex --poly -o "++fileProper++".tex"
                             system $ "pdflatex "++fileProper++".tex"
                             return ()
                        return ()
  return ()