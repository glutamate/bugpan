module Main where

import Text.Regex.Posix
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W
import Database
import TNUtils
import Query
import QueryMaker
import EvalM
import System.Environment
import System.Cmd
import System.Exit



import Data.List

type CodeWriterT m a= StateT (Int, [String]) (WriterT [String] m) a

indent n = do 
  modify $ \(ind,ss)->(ind+n,ss)
  
tell :: Monad m => String -> CodeWriterT m ()
tell s = do n <- fst `fmap` get
            lift $ W.tell [(replicate n ' ')++s]

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
modimport s = lift $ W.tell ["import "++s]

chomp :: String -> String
chomp s = dropWhile (==' ') s

execCodeWriterT :: Monad m => String -> CodeWriterT m () -> m String
execCodeWriterT modNm cw = do
  ss <- execWriterT $ execStateT cw (0,[])
  let (imps, rest) = partition ("import " `isPrefixOf`) ss
  return $ unlines (("module "++modNm++" where"):imps++rest)

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
                mkAnal rest)] $  do 
                     procQ writeQ $ unlines $ chomp q1 : map (tail) tablines
                     mkAnal rest

mkAnal ("":ss) = mkAnal ss
mkAnal ss = do
  let (para, rest) = span (\ln -> justText ln && (not $ null $ ln)) ss
  tellPrint "<p>"
  mapM tellPrint para
  tellPrint "</p>"
  mkAnal rest 
  

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

procTtest q testlines = do
  tell "tres <- ttest $ do"
  let q1= (chomp $ tail $ testlines!!0)
  let q2= (chomp $ tail $ testlines!!1)
  indent 3
  tellNmsTys
  tell $ "return (snd $ head $"++q1++", snd $ head $ "++q2++")"
  indent $ -3
  --tellPrint "<pre>"
  tellPrintNoLn $ "<pre>> t-test '"++q1++"', '"++q2++"'\n   p < "
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
                 tellPrintCode $ "> "++lhs ++ " = " ++ rhs
    | s =~ "^\\s*(\\w+)\\s*=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 tellPrintCode $ "> "++lhs ++ " = " ++ rhs
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
  (fileNm:rest) <- getArgs
  file <- lines `fmap` readFile fileNm
  let fileProper = head $ splitBy '.' fileNm
  let hsFile = fileProper++".hs"
  let htmlFile = fileProper++".html"
  code <- execCodeWriterT "Main" (writer file) 
  writeFile hsFile code
  ghcres <- system $ "ghc -O2 --make "++hsFile
  case ghcres of
    ExitFailure _ -> fail $ "ghc fail: "++show ghcres
    ExitSuccess -> system $ "./"++fileProper++" "++intercalate " " rest ++" >"++htmlFile
  return ()