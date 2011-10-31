module Main where


import PlotGnuplot
import NewSignal
import System.IO
import Query
import QueryUtils
import QueryTypes
import QueryPlots
import Math.Probably.FoldingStats

data DState = DState { noiseThreshold :: Double,
                       splitThreshold :: Double,
                       sess:: String,
                       templates :: [([Double], Signal Double)] } 

instance Show DState where
   show (DState n s sess _) = concat ["noise=", show n, ", split=", show s, ", session="++sess]

plotit h height x = do
  plines <- multiPlot unitRect x
  let cmdLines = "set terminal wxt size 700,"++show height++" noraise\n"++
               "set datafile missing \"NaN\"\n"++
                  (showMultiPlot plines)
                       
  --liftIO $ putStrLn cmdLines
  tellInteractivePlot  h cmdLines
--  addToCleanUp $ cleanupCmds $ map snd plines


main = interactivePlot $ \h-> do
--  let plot x = plotit h x
  prompt h $ DState 6.0 2.0 "" []


prompt h dst = do
  putStr "> "
  hFlush stdout
  s<- getLine
  dispatch h dst s

dispatch h dst ('n':' ':rest) = prompt h $ dst { noiseThreshold = read rest}
dispatch h dst ('s':' ':rest) = prompt h $ dst { splitThreshold = read rest}
dispatch h dst ('o':' ':rest) = prompt h $ dst { sess = rest}
dispatch h dst ('d':rest) = display h dst >> prompt h dst
dispatch h dst ('c':rest) = do templs <- go dst 
                               putStrLn $ show (length templs) ++ " templates"
                               prompt h dst {templates = templs}
dispatch h dst "q" = return ()
dispatch h dst "?" = do
   putStrLn "current state"
   print dst
   putStrLn "commands:"
   putStrLn " n [noiselevel]"
   putStrLn " s [splitlevel]"
   putStrLn " o [session]: open session"
   putStrLn " c: count"
   putStrLn " d: display"
   putStrLn " t i1 i2..: take spikes in group i1 and i2 etc."
   putStrLn " q: quit"
   prompt h dst
dispatch h dst s = putStrLn ("unknown command "++s) >> prompt h dst

go (DState nThr sThr sess _) = inApproxSession sess $ do
     ecVolts <- signalsDirect "ecVoltage"
     let noiseSD = sigSD ecVolts 
     let minIdx [] = -1
         minIdx xs = if (xs!!minimumIx xs)> sThr then -1 else minimumIx xs
     let putatives = (\v->v<(-nThr*noiseSD) || v>(nThr*noiseSD) ) ?? ecVolts
     io $ putStrLn $ "nputatives = "++show (length putatives)
     let rms = \t (templ, _, _ ) -> sqrt $ sumSig $ fmap (^2) $ templ - head (around (ev t) ecVolts)
     let aroundTime t = head $ limitSigs (-0.001) 0.001 $ around (ev t) ecVolts
     let update t templatesAndCounts (-1) = (aroundTime t, 1, [t]):templatesAndCounts
         update t templatesAndCounts ix = onIx ix templatesAndCounts 
                                               $ \(templ, count,ts) 
                                                     -> (fmap (/(count+1)) $ fmap (*count) templ + aroundTime t 
                                                         ,count+1, t:ts)
     let acc = \templatesAndCounts t -> update t templatesAndCounts $ minIdx $ map (rms t) templatesAndCounts
     let templates = foldl acc [] $ map fst putatives
     return $ map (\(tmpl, _, ts) -> (reverse ts, tmpl)) templates

display h (DState _ _ _ tmpls) = do
     let minv = foldl1 min $ map (snd . sigStat' minF) $ map snd tmpls
     let maxv = foldl1 max $ map (snd . sigStat' maxF) $ map snd tmpls
     let height = length tmpls * 100
     plotit h height $ Vplots $ map (Noaxis . YRange minv maxv . (:[]) . snd) tmpls
     return ()

ev = \t->[(t,())]

sumSig = foldSig (+) 0

sigSD ss = sqrt $ runStat meanF $ map (snd . sigStat' varF) ss