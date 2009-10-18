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

type CodeWriterT m a= StateT Int (WriterT [String] m) a

indent n = do 
  modify $ \ind->ind+n
  
tell :: Monad m => String -> CodeWriterT m ()
tell s = do n <- get
            lift $ W.tell [(replicate n ' ')++s]

tellPrint s = tell $ "io $ putStrLn $ "++show s

tellPrintCode s = tell $ "io $ putStrLn $ \"<pre>\\n\"++"++show s++"++\"</pre>\""

tellPrintSessionName = do
    tell "sessionIdentifier <- getSessionName"
    tell "io $ putStrLn $ \"<pre>\\n\"++\"> openSession \"++sessionIdentifier++\"</pre>\""

modimport :: Monad m => String -> CodeWriterT m ()
modimport s = lift $ W.tell ["import "++s]

chomp :: String -> String
chomp s = dropWhile (==' ') s

execCodeWriterT :: Monad m => String -> CodeWriterT m () -> m String
execCodeWriterT modNm cw = do
  ss <- execWriterT $ execStateT cw 0
  let (imps, rest) = partition ("import " `isPrefixOf`) ss
  return $ unlines (("module "++modNm++" where"):imps++rest)

mkAnal :: [String] -> CodeWriterT IO ()
mkAnal [] = return ()
mkAnal (('>':q):ss) = 
    let ind = indentOf q 
        (tablines, rest) = span (\ln-> not (justText ln) && 
                                                   indentMoreThan ind (tail ln)) ss
    in do if "table" `isPrefixOf` (chomp q)
             then do procTable q tablines
                     mkAnal rest
             else do procQ' $ concat $ chomp q : map (chomp . tail) tablines
                     mkAnal rest

mkAnal ss = do
  let (para, rest) = span justText ss
  tellPrint "<p>"
  mapM tellPrint para
  tellPrint "</p>"
  mkAnal rest
  

justText ('>':s) = False
justText s = True

indentOf = length . takeWhile (==' ')

indentMoreThan n = (>n) . indentOf

--todo:
-
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

procTable q tablines = do
  tellPrint "<table cellspacing = \"0\"><thead><tr>"
  tellPrint "<th>session</th>"
  tellPrint $ concatMap (\l-> "<th>"++tail (chomp l)++"</th>") tablines
  tellPrint "</tr></thead><tbody>"
  tell "tab <- inEverySession $ do"
  indent 10
  tellNmsTys
  tellPrint "<tr>"
  tell "sessionIdentifier <- getSessionName"
  tell "io $ putStrLn $ \"<td>\"++take 6 sessionIdentifier++\"</td>\""

  forM_ (tablines ) $ \ln -> do
                         tellPrint "<td>"
                         tell $ "askForLiterateTable $ "++chomp (tail ln)
                         tellPrint "</td>"
  tellPrint "</tr>"
  -- tell $ "io $ putStrLn $ "
  indent $ -10
  tellPrint "</tbody></table>"

  return ()



procQ' s 
    | s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 tell $ "storeAs "++show lhs ++" "++lhs
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
           do tell "inEverySession $ do"
              indent 3
              tellNmsTys
              tellPrintSessionName
    | s =~ "^\\s*openSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*openSession\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show sessnm++" $ do"
                 indent 3
                 tellNmsTys              
                 tellPrintCode $ "> openSession "++sessnm
    | s =~ "^\\s*close\\s*" = do tell "return ()"
                                 indent $ -3
    | s =~ "^\\s*break\\s*" = tellPrint "<div style=\"page-break-before: always\" />"
                                
    | otherwise = do tellPrintCode $ "> "++s
                     tell $ "askForLiterate $ "++s
                     tellPrint "<p />"


initHtml = 
           ["<html>",
            "<head>",
             "<style>",
             "thead th { border-bottom: 1px solid black; }",
             --"tbody  tr { border-top: 1px solid black; }",
             "table td, thead th { padding: 5px; }",
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
    ExitFailure _ -> return ghcres
    ExitSuccess -> system $ "./"++fileProper++" "++intercalate " " rest ++" >"++htmlFile
  return ()