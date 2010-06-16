module Main where

import Database
import Query
import System.Environment
import TNUtils
import EvalM
import System.Directory

import Control.Monad
import Control.Monad.Writer.Lazy
--import Language.Haskell.Interpreter
import QueryTypes
import Data.Char
--import System.Posix.Resource
import ValueIO
import PrettyPrint
import Numbers
import System.Cmd
import Format2
import Data.List
import System.Time
import System.Exit
import Data.Maybe
import System.Process
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal
import PlotGnuplot
import QueryUtils
import QueryPlots
import QueryMaker
import System.IO


main = do
  allArgs <- getArgs
  let (opts, args) = partition (beginsWithHyphen .&. notHyphenDigit) allArgs
  dispatch opts args

p .&. q = \x-> p x && q x

notHyphenDigit ('-':c:_) = not $ isDigit c
notHyphenDigit _ = True

preprocessQuery qs | "@=" `isInfixOf` qs = let (lhs, rhs) = span (/='@') $ qs
                                           in "\""++(filter (/=' ') lhs)++"\" "++rhs
                   | otherwise = qs

--matched parens, never negative level
--escape quotes?
validateQuery q = checkParens q 0

checkParens :: String -> Int -> Bool
checkParens [] 0 = True
checkParens [] _ = False
checkParens ('(':s) n = checkParens s (n+1)
checkParens (')':s) n | n < 1 = False
                      | otherwise = checkParens s (n-1)
checkParens (_:s) n = checkParens s n

withNothing x = (x,Nothing)                 


dispatch opts ("ask1":sessNm:queryStr':_) = do
  queryStr <- if "-f" `elem` opts 
                 then preprocessQuery `fmap` readFile queryStr'
                 else return $ preprocessQuery queryStr'
  when (not $ validateQuery queryStr) $
       fail $ "cannot validate query string"
  let sha = take 50 . showDigest . sha512 . BS.pack $ map c2w queryStr
  --putStrLn sha
  whenM (not `fmap` doesFileExist (bugpanRootDir./"queryCache/"++sha)) 
        (compileQuery opts sha queryStr)
  longSessNm <- resolveApproxSession root sessNm
  if ("-p" `elem` opts)
     then system $ bugpanRootDir./"queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " opts)++" +RTS -p"
     else system $ bugpanRootDir./"queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " opts)
  

  return ()

dispatch opts ("askall":q:_) = do
  sesns <- getSessionInRootDir root
  forM_ sesns $ \sNm -> do
    putStr $ "bugsess ask1 "++sNm++" '"++q++"': "
    hFlush stdout
    system $ "bugsess ask1 "++sNm++" '"++q++"'"
           

dispatch opts ("ask":sessNm:rest) = do
  if sessNm == "all"
     then dispatch opts $ "askall":rest
     else dispatch opts $ "ask1":sessNm:rest

dispatch _ ("convert1":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  sigs <- getDirContents $ (oneTrailingSlash $ baseDir sess)++"signals"
  let path  = (oneTrailingSlash $ baseDir sess)++"signals/"
  forM_ sigs $ \sig -> do 
    vs<-loadUntyped1 $ path++sig
    createDirectory $ path++sig++"_conv"
    forM_ vs $ \(nm,sigv) -> do let fp = path++sig++"_conv/"++nm
                               --print fp
                                putStrLn $ "saving "++show sigv++ " in "++fp
                                saveBinary fp [sigv]
  --print sigs
      where loadUntyped1 :: FilePath -> IO [(String,V)]
            loadUntyped1 fp = do 
              ifM (doesDirectoryExist fp)
                  (do fnms <- getSortedDirContents fp
                      xs <- forM fnms $ \fn-> do b<-loadBinary $ fp++"/"++fn
                                                 return (fn,b)
                      return $ xs)
                  ((print $ "dir not found:" ++fp) >> return [])

dispatch _ ("convert2":sessNm:_) = do
  sessOld <- loadApproxSession root sessNm
  sessNew <- cloneSession sessOld "_fmt3" 3
  forM_ ["signals", "events", "durations"] $ \kind -> do
                                   sigs <- getDirContents $ (oneTrailingSlash $ baseDir sessOld)++kind
                                   let path  = (oneTrailingSlash $ baseDir sessOld)++kind++"/"
                                   let pathN  = (oneTrailingSlash $ baseDir sessNew)++kind++"/"
                                   --putStrLn $ "kind="++kind
                                   forM_ sigs $ \sig -> do 
                                                       --putStrLn $ "sig="++sig
                                                       vs<-loadUntyped1 $ path++sig
                                                       createDirectory $ pathN++sig
                                                       forM_ vs $ \(nm,sigv) -> do let fp = pathN++sig++"/"++nm
                                                                                   putStrLn $ "saving "++show sigv++ " in "++fp
                                                                                   saveVs fp sigv
      where loadUntyped1 :: FilePath -> IO [(String,[V])]
            loadUntyped1 fp = do 
              ifM (doesDirectoryExist fp)
                  (do fnms <- getSortedDirContents fp
                      xs <- forM fnms $ \fn-> do b <- loadBinary $ fp++"/"++fn
                                                 --return $ idLstOldFmtV b
                                                 --putStrLn $ fn ++ show b
                                                 return (fn,map oldVtoV b)
                      return $ xs)
                  ((print $ "dir not found:" ++fp) >> return [])

dispatch o ("compactall":sessPrefix:_) = do
  sesns <- getSessionInRootDir root
  forM_ sesns $ \sNm -> do
      when (sessPrefix `isPrefixOf` sNm) $ dispatch o ["compact", sNm]


dispatch _ ("compact":sessNm:_) = do
  system $ "bugsess compact_1 "++sessNm
  system $ "bugsess compact_2 "++sessNm
  return ()

dispatch _ ("compact_1":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         allfnms <- getSortedDirContents $ path++nm
         let fnms = filter (not . ("compacted" `isPrefixOf`)) allfnms
--         print2 ("files to compact in "++nm) fnms
         
         let fileNoMax = foldl (max) 0 $ catMaybes $ map (safeRead . (drop 9)) $ filter ("compacted" `isPrefixOf`) allfnms
--         print2 "filemaxno " fileNoMax
         xs <- forM fnms $ \fn->loadBinaryStrict $ path++nm++"/"++fn                                    
         let vs = sortVs $ idLstV $ concat xs
         saveBinary (path++nm++"/compacted"++show (fileNoMax + idInt 1)) vs
         --putStrLn $ nm ++ ": "++ppVal (ListV vs)

dispatch _ ("compact_2":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  forM_ ["events", "durations"] $ \kind -> do
       nms <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
       let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
       forM_ nms $ \nm -> do 
         fnms <- getSortedDirContents $ path++nm
         forM fnms $ \fn-> if ("compacted" `isPrefixOf` fn)
                              then return () --print "skipping"
                              else removeFile $ path++nm++"/"++fn
                    
dispatch opts ("list":_) = do
  sesns <- getSessionInRootDir root
  forM_ sesns $ \sNm -> do
      putStr $ sNm
      inSessionNamed sNm $ do 
                          --prg <- durations "program" ""
                          Session bdir _ <- getSession
                          let vfile = (oneTrailingSlash bdir ++ "sessionFormatVersion")
                          ifM (liftIO $ doesFileExist vfile)
                              ( do v <- liftIO $ readFile vfile
                                   liftIO. putStr $ " (v"++v++")")
                              (return ())
                          when ("-m" `elem` opts) $ do
                             modNm <- durations "moduleName" ""
                             liftIO . putStr $ ": "++(showDiffModules $ map snd modNm)
      putStrLn ""

dispatch opts ("filter":rest) = do
  dispatch opts ("filter1":rest)

dispatch opts ("filter1":queryStr':_) = do
  sesns <- getSessionInRootDir root
  setCurrentDirectory bugpanRootDir
  queryStr <- if "-f" `elem` opts 
                 then preprocessQuery `fmap` readFile queryStr'
                 else return $ preprocessQuery queryStr'
  when (not $ validateQuery queryStr) $
       fail $ "cannot validate filter string"
  let sha = take 50 . showDigest . sha512 . BS.pack $ map c2w queryStr
  --putStrLn sha
  whenM (not `fmap` doesFileExist (bugpanRootDir./"queryCache/"++sha)) 
        (compileQuery opts sha queryStr)
  --longSessNm <- resolveApproxSession root sessNm
  forM_ sesns $ \sNm -> do
    longSessNm <- resolveApproxSession root sNm
    system $ bugpanRootDir./"queryCache/"++sha++" "++ longSessNm++" "++(intercalate " " (("-filter"):opts))
  

  return ()


dispatch opts ("show":sessNm:_) = do
  sess <- loadApproxSession root sessNm
  tps <- sessionTypes sess
  when (not $ "-t" `elem` opts) $ do
    (t1,t2) <- read `fmap` readFile (baseDir sess++"/tStart")
    let t0 = TOD t1 t2
    putStrLn $ "Start time: "++ show t0
    inSession sess $ do 
       --prg <- durations "program" ""
      modNm <- durations "moduleName" ""
      liftIO . putStrLn $ "Modules run: "++(showDiffModules $ map snd modNm)
  putStrLn "Values:"
  forM_ tps $ \(nm, ty) -> do
    putStrLn $ "\t"++(unCap nm)++" :: "++pTy ty
        where pTy (PairT (PairT (NumT (Just RealT)) (NumT (Just RealT))) t) = "Duration "++ppType t
              pTy (PairT (NumT (Just RealT)) t) = "Event "++ppType t
              pTy t = ppType t

dispatch opts ("mvevs":sessNm:oldNm:newNm:_) = do
  longSessNm <- resolveApproxSession root sessNm
  renameDirectory (bugpanRootDir./"sessions" ./ longSessNm ./ "events" ./ oldNm) 
                  (bugpanRootDir./"sessions" ./ longSessNm ./ "events" ./ newNm)
  return ()

dispatch opts ("lnsess":newDir:_) = do
  ifM (doesDirectoryExist $ bugpanRootDir./newDir)
      (do system "rm /var/bugpan/sessions"
          system $ "ln -s /var/bugpan/"++newDir++" /var/bugpan/sessions"
          return ())
      (putStrLn $ "directory does not exist: /var/bugpan/"++newDir)


dispatch opts ("screenplot":sessNm:sigNm:sigIdx) = do
  inApproxSession sessNm $ do
            sig <- signalsDirect sigNm
            case safeHead sigIdx >>= safeRead of
                 Just i  -> io $ gnuplotOnScreen $ [sig!!i]
                 Nothing -> io $ gnuplotOnScreen $ sig
  return ()                                                                    

dispatch opts ("plotsigs":sessNm:sigNm:_) = do
  qres <- inApproxSession sessNm $ do
            sig <- signalsDirect sigNm
            return $ plotManySigs sig
  qreply <- qReply qres opts
  case dropPrefix "file:///var/bugpan/www/" qreply of
    ("",s) -> cond [("-o" `elem` opts,  do
                       system $ "gnome-open "++qreply
                       return ()),
                    ("-s" `elem` opts, do 
                       system "ifconfig eth0 | perl -n -e 'if (m/inet addr:([\\d\\.]+)/g) { print $1 }' | cat >/tmp/my_ip_address" 
                       ip <- readFile "/tmp/my_ip_address"
                       putStrLn $ "http://"++ip++"/"++s
                       system $ "chmod -x /var/bugpan/www/"++s
                       system $ "sed -i 's/\\/var\\/bugpan\\/www\\//\\//g' /var/bugpan/www/somewhere/*.html"
                       system $ "chmod -x /var/bugpan/www/somewhere/*.png"
                       return ())] $ putStrLn qreply
                       
    _ -> putStrLn qreply

  --putStrLn qreply

  return ()

dispatch opts ("mkdur":sessNm:durnm:rest) = do
  inApproxSession sessNm $ do
            case rest of
              [] -> storeAsOvwrt durnm (dur ()) >> return ()
              val:[] -> case safeRead val of
                          Just x -> storeAsOvwrt durnm (durd x) >> return ()
                          Nothing -> storeAsOvwrt durnm (dur val) >> return ()
  return ()

dispatch opts ("kill":sessNm:_) = do
  s <- loadApproxSession root sessNm
  deleteSession s

dispatch opts ("addev":sessNm:evNm:rest) = do
  qres <- inApproxSession sessNm $ do
            sess <- getSession
            tnow <- io $ getClockTime
            let t0 = diffInS tnow $ tSessionStart sess
            case rest of
              [] -> storeAsAppend evNm [(t0::Double,())] >> return ()
              val:[] -> case safeRead val of
                          Just x -> storeAsAppend evNm [(t0::Double,x::Double)] >> return ()
                          Nothing -> storeAsAppend evNm [(t0::Double,val)] >> return ()          
  return ()

dispatch opts ("check":sessSpec:_) = 
    if sessSpec == "all"
       then do  sesns <- getSessionInRootDir root
                forM_ sesns (checkit False)
       else checkit True sessSpec 

    where checkit verbose sessNm  = do
            sess <- loadApproxSession root sessNm
            --tps <- sessionTypes sess
            when (not verbose) $ putStr $ sessNm++ ": "
            --when verbose $ putStr $ "types: "
            hFlush stdout
            --when verbose $ print $ length $ concatMap show tps
            lens<- forM ["signals", "events", "durations"] $ \kind -> do
                                   sigs <- getDirContents $ (oneTrailingSlash $ baseDir sess)++kind
                                   let path  = (oneTrailingSlash $ baseDir sess)++kind++"/"
                                   when verbose $ putStrLn $ "testing "++ show sigs
                                   forM sigs $ \sig -> do 
                                     when verbose $ putStr $ path++sig++" "
                                     fnms <- getSortedDirContents $ path++sig
                                     fTT <- fileTypeTag $ path ./ sig ./ (head fnms)
                                     utevs <- forM fnms $ \fn-> liftIO $ loadVs $ path++sig++"/"++fn
                                     let sm = (sum $ map length utevs) + (length $ show fTT)
                                     when verbose $ do putStrLn $ show sm
                                                       hFlush stdout
                                     return $ sm
            putStrLn $ "OK ("++show ((sum $ map sum lens))++")"

dispatch os ss = putStrLn $ unlines ["",
              "Unknown command: bugsess "++intercalate " " os++" "++intercalate " " ss,
              "",
              "Bugsess: Manage bugpan sessions",
              "",
              "\tbugsess ask1 {session} {query}",
              "\tbugsess show {session}",
              "\tbugsess list",
              "\tbugsess filter1 {query}",
              "\tbugsess compact {session}",
              "\tbugsess compactall {session prefix}",
              "\tbugsess convert2 {session}",
              "\tbugsess convert1 {session}",
              "\tbugsess kill {session}",
              "\tbugsess lnsess {sessionDir}",
              "\tbugsess mvevs {session} {oldName} {newName}",
              "\tbugsess mkdur {session} {durationName} [value]",
              "\tbugsess addev {session} {eventName} [value]",
              "\tbugsess plotsigs {session} {signalName}",
              "\tbugsess screenplot {session} {signalName} [index]",
              "\tbugsess askall {query}",
              "\tbugsess check {session}|all"
 ]

-- find . -name "compacted" -exec rm -rf {} \;

showDiffModules :: [String] -> String
showDiffModules mods = intercalate ", " . map f $ group mods
    where f mods =(unCap $head mods) ++ "("++ (show $ length mods)++")"

idLstV :: [V] -> [V]
idLstV = id


myfun ("foo") = True

