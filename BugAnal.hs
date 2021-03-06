{-# LANGUAGE ViewPatterns #-}

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
import System.IO
 
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
    tell "sessionStart <- getSessionStart"
    tellBeginCode
    tell "io $ putStrLn $ \"openSession \"++sessionIdentifier"
    tellEndCode
tellEverywheres = do
  dfns <- snd `fmap` get
  --io $ print dfns
  mapM_ (tell . ("let "++)) $ reverse $ filter (not . isRebase) dfns 
tellRebase = do
  dfns <- snd `fmap` get
  --io $ print dfns
  mapM_ tell $ reverse $ filter isRebase dfns 


isRebase = ("rebaseRelativeTo " `isPrefixOf`)

modimport :: Monad m => String -> CodeWriterT m () 
modimport s = tellNoindent $ "import "++s

chomp :: String -> String
chomp s = dropWhile (==' ') s

dropInitAngle = dropWhile (=='>')

ensurePostfix ext s | ext `isSuffixOf` s = s
                    | otherwise = s++ext

mkAnal :: [String] -> CodeWriterT IO ()
mkAnal [] = return ()
mkAnal (cl@('%':_):ss) = tellPrint cl >> mkAnal ss
mkAnal ((">"):ss) = mkAnal ss
mkAnal (('>':q):ss) = 
    let writeQ = head q /= '>'
        q1 = if writeQ then q else tail q
        ind = indentOf q1
        (tablines, rest) = span (\ln-> not (justText ln) && 
                                                   indentMoreThan ind (dropInitAngle ln)) ss
    in cond [("table" `isPrefixOf` (chomp q1), do
                procTable (words q1) tablines
                mkAnal rest), 
             ("importBanal" `isPrefixOf` (chomp q1), do
                let fnm = ensurePostfix ".banal" $ (words q1)!!1
                lines <- fmap lines $ liftIO $ readFile fnm
                mkAnal $ lines ++ rest),
             ("chainmap" `isPrefixOf` (chomp q1), do
                procChainMap (words q1)
                mkAnal rest),
             ("do " `isPrefixOf` (chomp q1), do
                tell $ drop 3 q1
                mkAnal rest),
             ("chains" `isPrefixOf` (chomp q1), do
                procLoadChain (words q1)
                mkAnal rest),
             ("chain" `isPrefixOf` (chomp q1), do
                procChain (words q1)
                mkAnal rest),
             ("t-test" `isPrefixOf` (chomp q1), do
                procTtest (words q1) tablines
                mkAnal rest),
             (":=" `isInfixOf` (chomp q1), do
                procCalc q1 tablines
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
                mkAnal rest)] $  do 
                     procQ writeQ $ unlines $ chomp q1 : map (dropInitAngle) tablines
                     mkAnal rest

mkAnal ("":ss) = mkAnal ss
mkAnal ss = do
  let (para, rest) = span (\ln -> justText ln && (not $ null $ ln) && (not $ '%' == head ln)) ss
  tellPrint ""
  mapM tellPrint para
  tellPrint ""
  mkAnal rest 
  

--BUG: should not saw new: at beginning if run more than once
runGoal :: String -> String -> Int -> IO ()
runGoal sessNm goal n = do
  system $ "ghc -O2 -DNODAQ --make "++goal
  forM [1..n] $ const $ system $ "./"++goal++" "++sessNm
  return ()

           

justText ('>':s) = False
--justText ("") = False
justText s = True

indentOf = length . takeWhile (==' ')

indentMoreThan n = (>n) . indentOf


allNmsTypes = do
  sesns <- getSessionInRootDir root
  getNamesAndTypes sesns

tellNmsTys = do
  tellRebase
  nmtys <- liftIO $ allNmsTypes 
  forM_ nmtys $ \(nm,ty) -> do
               if ty == SignalT (NumT (Just RealT))
                  then tell $ unCap nm ++ " <- signalsDirect \""++ nm++"\";"
                  else tell $ concat [unCap nm, " <- "++typeToKind ty,
                                                       " \""++ nm++"\" ",
                                                       (typeToProxyName $ unWrapT ty), ";"
                                                      ]
  tellEverywheres

procChain [_, vname, cname, cnum, parnm, fstart, fstop] = do
  tell $ vname ++"<- loadChain "++unwords [show cname, show parnm, 
                                           cnum, "("++fstart++","++fstop++")"
                                           ]
  return ()

procLoadChain [_, cname, cnum, fstart, fstop, takeN, dropN] = do
  parstrs <- fmap read $ liftIO $ readFile (cname++"_parnames.mcmc")
  let varnm = last $ splitBy '/' cname
  tell $ varnm ++ " <- loadChainMap "++unwords [show cname,cnum, 
                                                "("++fstart++","++fstop++")", takeN, dropN]
  forM_ parstrs $ \parnm-> do
    tell $ "let "++parnm++" = Samples $ "++varnm++"!!!"++show parnm
  return ()

procChainMap [_, vname, cname, cnum, fstart, fstop, thn] = do
  tell $ vname ++"<- loadChainMap "++unwords [show cname,cnum, 
                                              "("++fstart++","++fstop++")", thn]
  return ()


procTtest qs [tl1, tl2] = do
  let mfiltr = case qs of
                 "table":"where":tl -> Just $ unwords tl
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
                 "table":"where":tl -> Just $ unwords tl
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

beforeTilde, afterTilde :: [Char] -> [Char]
beforeTilde = head . splitByMany " ~ " 

afterTilde ln = case splitBy '~' ln of
                   [cs] -> cs
                   _:cs:_ -> cs


procTable qs tablines = do
--  io $ print "table!"
--  io $ print qs
  let mfiltr = case qs of
                 "table":"where":tl -> Just $ unwords tl
                 _ -> Nothing
  tellPrintNoLn "\\begin{tabular}{ l"
  tellPrintNoLn $ concatMap (\l-> " || c") tablines
  tellPrint " }"
  tellPrintNoLn "session"
  tellPrintNoLn $ concatMap (\l-> " & "++(beforeTilde $ tail $ chomp l)) tablines
  tellPrint " \\\\"
  tellPrint "\\hline"
  tell "inEverySession_ $ do"
  indent 3
  tellNmsTys
  when (isJust mfiltr) $ do tell $ "when (not . null $ "++fromJust mfiltr++") $ do"
                            indent 3                       
  --tellPrint "<tr>"
  tell "sessionIdentifier <- getSessionName"
  tell "sessionStart <- getSessionStart"
  tell "io $ putStrLn $ take 6 sessionIdentifier"
  forM_ (tablines ) $ \ln -> do
                         tellPrintNoLn " & "
                         tell $ "askForLiterateTable $ "++(afterTilde $ chomp $ tail ln)
  tellPrint " \\\\"
  -- tell $ "io $ putStrLn $ "
  if (isJust mfiltr) 
     then indent $ -6
     else indent $ -3
  tellPrint "\\hline"
  tellPrint "\\end{tabular}"

  return ()


procCalc q tablines = do
  let (chomp -> nm):rhs:_ = splitByMany ":=" q
  let (xform,query) = if "$$" `isInfixOf` rhs
                          then let x:q:_ = splitByMany "$$" rhs in (x,q)
                          else ("id", rhs)
  let mfiltr = case filter (elem "where" . words) tablines of
                      whereln:_ -> Just (intercalate " " $ tail $ words whereln)
                      _ -> Nothing
  
  if not $ isJust mfiltr 
     then tell $ nm ++" <- fmap ("++xform++") $ inEverySession $ do"
     else tell $ nm ++" <- fmap (("++xform++") . catMaybes) $ inEverySession $ do"
  indent 3
  tellNmsTys
  if isJust mfiltr 
     then do tell $ "if (not . null $ "++fromJust mfiltr++")"
             tell $ "   then return $ Just $ "++ query
             tell $ "   else return Nothing"
     else tell $ "return $ "++ query
  indent $ -3
  --tell $ "askForLiterateIO "++nm
  return ()


procQ writeQ s 
    | s =~ "^\\s*everywhere\\s*(.+)" = 
           let [[_, defn]] = s =~ "^\\s*everywhere\\s*(.+)"
           in do --io $ print s
                 modify $ \(i, ss) -> (i, defn:ss)
                 --io $ putStrLn $ "remembering "++defn
                 when writeQ $tellPrintCode $ "everywhere "++defn
    | s =~ "^\\s*rebase\\s*(.+)" = 
           let [[_, defn]] = s =~ "^\\s*rebase\\s*(.+)"
           in do --io $ print s
                 modify $ \(i, ss) -> (i, ("rebaseRelativeTo "++show defn):ss)
                 --io $ putStrLn $ "remembering "++defn
                 when writeQ $tellPrintCode $ "everywhere "++defn
    | s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*(\\w+)\\s*@=\\s*(.+)")::[[String]]     
           in do tell $ "let "++lhs ++" = "++rhs
                 tell $ "storeAsOvwrt "++show lhs ++" "++lhs
                 when writeQ $ tellPrintCode $ lhs ++ " = " ++ rhs
    | s =~ "^\\s*([^=]+)\\s+=\\s+(.+)" = 
           let [[all, lhs, rhs]] = (s =~ "^\\s*([^=]+)\\s*=\\s*(.+)")::[[String]]     
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
           in do tellPrintCode $ "inSessionsWhere "++filtr
                 tell "inEverySession_ $ do"
                 indent 3
                 tellNmsTys  
                 --tellPrintSessionName
                 --tell $ " io $ print (not . null $ "++filtr++")"
                 tell $ "when (not . null $ "++filtr++") $ do"
                 indent 3
                 tellPrintSessionName
    | s =~ "^\\s*openSession\\s*(.+)" = 
           let [[all, sessnm]] = (s =~ "^\\s*openSession\\s*(.+)")::[[String]]
           in do tell $ "inApproxSession "++show sessnm++" $ do"
                 indent 3
                 tellNmsTys              
                 when writeQ $ tellPrintCode $ "openSession "++sessnm
    | s =~ "^\\s*importHs\\s*(.+)" = 
           let [[all, modnm]] = (s =~ "^\\s*importHs\\s*(.+)")::[[String]]
           in modimport modnm
    | s =~ "^\\s*plotSize\\s*(.+)x(.+)" = 
           let [[all, w, h]] = (s =~ "^\\s*plotSize\\s*(.+)x(.+)")::[[String]]
           in tell $ "plotSize "++w++" "++h
    | s =~ "^\\s*fontSize\\s*(.+)" = 
           let [[all, h]] = (s =~ "^\\s*fontSize\\s*(.+)")::[[String]]
           in tell $ "fontSize "++h
    | s =~ "^\\s*close\\s*" = do tell "return ()"
                                 indentAbs $ 3
    | s =~ "^\\s*break\\s*" = tellPrint "\\pagebreak"
                                
    | chomp s == "" = return ()
    | otherwise = do when writeQ $ do
                       tellBeginCode
                       tellPrint s 
                       tellEndCode
--                       tellPrintNoLn "| => |"
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
    ["\\documentclass[11pt]{article}",
     "%include lhs2TeX.fmt",
     "%include polycode.fmt",
     "\\usepackage[a4paper, top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}",
     "\\usepackage{graphicx}",
     "\\begin{document}"]

endHtml = 
      ["\\end{document}"]



writer s = do
  modimport "Database"
  modimport "Data.Maybe"
  modimport "QueryTypes"
  modimport "QueryUtils"
  modimport "Query"
  modimport "QueryPlots"
  modimport "PlotGnuplot"
  modimport "Data.List"
  modimport "Data.Ord"
  modimport "TNUtils"
  modimport "NewSignal"
  modimport "Control.Monad"
  modimport "StatsModel"
  modimport "FitGnuplot"
  modimport "Locust"
--  modimport "Control.DeepSeq"
  modimport "Math.Probably.FoldingStats"
  modimport "Math.Probably.GlobalRandoms"
  modimport "Math.Probably.Sampler"
  modimport "ReactiveDistributions"
  modimport "Math.Probably.Distribution"

  tell "main = do"
  indent 3
  when (('\\'/=) $ head $ head $ s) $
        mapM_ tellPrint initHtml
  mkAnal s
  indentAbs 3
  when (('\\'/=) $ head $ head $ s) $
        mapM_ tellPrint endHtml
  tell "return ()"
main = do 
  allArgs <- getArgs -- (fileNm:rest)
  let (opts, fileNm:rest) = partition beginsWithHyphen allArgs
  let fullFileNm = if '.' `elem` fileNm
                      then fileNm
                      else fileNm++".banal"
  file <- lines `fmap` readFile fullFileNm
  let fileProper = if '.' `elem` fileNm
                      then head $ splitBy '.' fileNm
                      else fileNm
  let hsFile = fileProper++".hs"
  let lhsFile = fileProper++".lhs"
  code <- execCodeWriterT "Main" (writer file) 
  writeFile hsFile code
  ghcres <- if ("-p" `elem` opts)
               then system $ "ghc -O2 -prof -auto-all --make "++hsFile
               else system $ "ghc -O2 -package probably --make "++hsFile
  let profOpts =  if ("-p" `elem` opts)
                    then " +RTS -p"
                    else ""
  case ghcres of
    ExitFailure _ -> fail $ "ghc fail: "++show ghcres
    ExitSuccess -> do 
                        system $ "./"++fileProper++" "++intercalate " " ((opts\\["-o", "-a"])++rest) ++" >"++lhsFile++profOpts
                        unless ("-nopdf" `elem` opts) $ do
--                          print "generating pdf..."
--                          hFlush stdout
                          system $ "lhs2TeX --poly -o "++fileProper++".tex "++fileProper++".lhs"
                          system $ "pdflatex -interaction=batchmode "++fileProper++".tex"
                          return ()
                        return ()
  return ()