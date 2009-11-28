module Main where

import Text.Regex.Posix
import Database
import TNUtils
import Query
import QueryMaker
import EvalM
import System.Environment
import System.Cmd
import System.Exit
import Data.List
import Control.Monad.State.Lazy
 
  

tellPrint s = tell $ "io $ putStrLn $ "++show s
tellPrintNoLn s = tell $ "io $ putStr $ "++show s

tellPrintCode s = tell $ "io $ putStrLn $ \"<pre>\"++"++show s++"++\"</pre>\""

tellPrintSessionName = do
    tell "sessionIdentifier <- getSessionName"
    tell "io $ putStrLn $ \"<pre>\"++\"> openSession \"++sessionIdentifier++\"</pre>\""

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
                procTable q1 tablines
                mkAnal rest), 
             ("t-test" `isPrefixOf` (chomp q1), do
                procTtest q1 tablines
                mkAnal rest),
             ("defineSession" `isPrefixOf` (chomp q1), do
                liftIO $ case words q1 of
                  defS:sessNm:goal:_ -> case last defS of
                                          '!' -> do
                                            s <- loadApproxSession sessNm root
                                            deleteSession s
                                            runGoal ("new:"++sessNm) goal
                                          '+' -> ifM (existsSession sessNm root)
                                                     (runGoal sessNm goal)
                                                     (runGoal ("new:"++sessNm) goal)
                                          _ -> whenM (not `fmap` existsSession sessNm root) $
                                                         runGoal ("new:"++sessNm) goal
                  _ -> return ()
                return () )] $  do 
                     procQ writeQ $ unlines $ chomp q1 : map (tail) tablines
                     mkAnal rest

mkAnal ("":ss) = mkAnal ss
mkAnal ss = do
  let (para, rest) = span (\ln -> justText ln && (not $ null $ ln)) ss
  tellPrint "<p>"
  mapM tellPrint para
  tellPrint "</p>"
  mkAnal rest 
  

runGoal :: String -> String -> IO ()
runGoal sessNm goal = do
  system $ "ghc -O2 --make "++goal
  system $ "./"++goal++" "++sessNm
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

procTtest q [tl1, tl2] = do
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

procTtest q [tl] = do
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

procTable q tablines = do
  tellPrint "<table cellspacing = \"5\"><thead><tr>"
  tellPrint "<th>session</th>"
  tellPrint $ concatMap (\l-> "<th>"++tail (chomp l)++"</th>") tablines
  tellPrint "</tr></thead><tbody>"
  tell "inEverySession_ $ do"
  indent 3
  tellNmsTys
  tellPrint "<tr>"
  tell "sessionIdentifier <- getSessionName"
  tell "io $ putStrLn $ \"<td align=center>\"++take 6 sessionIdentifier++\"</td>\""

  forM_ (tablines ) $ \ln -> do
                         tellPrint "<td align=center>"
                         tell $ "askForLiterateTable $ "++chomp (tail ln)
                         tellPrint "</td>"
  tellPrint "</tr>"
  -- tell $ "io $ putStrLn $ "
  indent $ -3
  tellPrint "</tbody></table>"

  return ()



procQ writeQ s 
    | s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 tell $ "storeAsOvwrt "++show lhs ++" "++lhs
                 when writeQ $ tellPrintCode $ "> "++lhs ++ " = " ++ rhs
    | s =~ "^\\s*(\\w+)\\s*=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 when writeQ $ tellPrintCode $ "> "++lhs ++ " = " ++ rhs
    | s =~ "^\\s*openSession\\s*$" = 
           do tell "inSessionFromArgs $ do"
              indent 3
              tellNmsTys
              tellPrintSessionName
    | s =~ "^\\s*inEverySession\\s*$" = 
           do tell "inEverySession_ $ do"
              indent 3
              tellNmsTys
              tellPrintSessionName
    | s =~ "^\\s*openSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*openSession\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show sessnm++" $ do"
                 indent 3
                 tellNmsTys              
                 tellPrintCode $ "> openSession "++sessnm
    | s =~ "^\\s*importHs\\s*(.+)" = 
           let [[all, modnm]] = (s =~ "^\\s*importHs\\s*(.+)")::[[String]]
           in modimport modnm
    | s =~ "^\\s*plotSize\\s*(.+)x(.+)" = 
           let [[all, h, w]] = (s =~ "^\\s*plotSize\\s*(.+)x(.+)")::[[String]]
           in tell $ "plotSize "++h++" "++w
    | s =~ "^\\s*close\\s*" = do tell "return ()"
                                 indent $ -3
    | s =~ "^\\s*break\\s*" = tellPrint "<div style=\"page-break-before: always\" />"
                                
    | s =~ "^\\s*everywhere\\s*(.+)" = 
           let [[_, defn]] = s =~ "^\\s*everywhere\\s*(.+)"
           in do --io $ print s
                 modify $ \(i, ss) -> (i, defn:ss)
                 --io $ putStrLn $ "remembering "++defn
                 when writeQ $tellPrintCode $ "> everywhere "++defn
    | chomp s == "" = return ()
    | otherwise = do when writeQ $ tellPrintCode $ unlines $ map ("> "++) (lines s)
                     tell $ "askForLiterate $ "++(concat $ map chomp $ lines s)
                     tellPrint "<p />"


initHtml = 
           ["<html>",
            "<head>",
             "<style>",
             "table { border-top: 1px solid black; ",
             "        border-bottom: 1px solid black;  }",
             "thead th { border-bottom: 1px solid black; }",
             --"tbody  tr { border-top: 1px solid black; }",
             --"table td, thead th { cell-spacing: 5px; }",
             "</style>",
            "</head><body>"]


endHtml = 
      ["</body></html>"]



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
  mapM tellPrint initHtml
  mkAnal s
  mapM tellPrint endHtml
  tell "return ()"
main = do 
  allArgs <- getArgs -- (fileNm:rest)
  let (opts, fileNm:rest) = partition beginsWithHyphen allArgs
  file <- lines `fmap` readFile fileNm
  let fileProper = head $ splitBy '.' fileNm
  let hsFile = fileProper++".hs"
  let htmlFile = fileProper++".html"
  code <- execCodeWriterT "Main" (writer file) 
  writeFile hsFile code
  ghcres <- if ("-p" `elem` opts)
               then system $ "ghc -O2 -prof -auto-all --make "++hsFile
               else system $ "ghc -O2 --make "++hsFile
  case ghcres of
    ExitFailure _ -> fail $ "ghc fail: "++show ghcres
    ExitSuccess -> if "-a" `elem` opts 
                      then do
                        sessNms <-  getSessionInRootDir "/var/bugpan/sessions/"
                        forM_ sessNms $ \sess -> do
                             let cmd = if ("-p" `elem` opts) 
                                          then "./"++fileProper++" "++sess++" >"++fileProper++take 6 sess++".html +RTS -p"
                                          else "./"++fileProper++" "++sess++" >"++fileProper++take 6 sess++".html"
                             putStrLn cmd
                             system cmd
                      else do 
                        if ("-p" `elem` opts)
                           then system $ "./"++fileProper++" "++intercalate " " ((opts\\["-o", "-a"])++rest) ++" >"++htmlFile++ " +RTS -p"
                           else system $ "./"++fileProper++" "++intercalate " " ((opts\\["-o", "-a"])++rest) ++" >"++htmlFile
                        when ("-o" `elem` opts) $ do
                             system $"gnome-open "++htmlFile
                             return ()
                        return ()
  return ()