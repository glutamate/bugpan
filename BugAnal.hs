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
mkAnal (('>':q):ss) = do
  procQ' q
  mkAnal ss
mkAnal ss = do
  let (para, rest) = span justText ss
  tellPrint "<p>"
  mapM tellPrint para
  tellPrint "</p>"
  mkAnal rest
  

justText ('>':s) = False
justText s = True


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
-- > to not include query in output
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
              tell "sessionIdentifier <- getSessionName"
              tell "io $ putStrLn $ \"<pre>\\n\"++\"> openSession \"++sessionIdentifier++\"</pre>\""
    | s =~ "^\\s*openSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*openSession\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show sessnm++" $ do"
                 indent 3
                 tellNmsTys              
                 tellPrintCode $ "> openSession "++sessnm
    | s =~ "^\\s*close\\s*" = do tell "return ()"
                                 indent $ -3
    | otherwise = do tellPrintCode $ "> "++s
                     tell $ "askForLiterate $ "++s
                     tellPrint "<p />"




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
  mkAnal s
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