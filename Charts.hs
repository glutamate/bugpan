{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Charts where

--import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart hiding (Plot, Legend, toPlot, ToPlot) 
import qualified Graphics.Rendering.Chart as C 
import Graphics.Rendering.Chart.Gtk
import Control.Concurrent
import Data.Accessor

-- data Col = Blue | Black | Red

data AxisNames = L | R | T | B deriving (Eq, Show)

instance Eq Color where
    c1 == c2 = False

data PlotStyle = Lines | FilledCircles | LineWidth Double | Legend String | Col Color deriving (Eq)

data GraphStyle = Title String | Axes [AxisNames] | Height Int | Width Int deriving (Eq, Show)

data Plot = Plot [(Double,Double)] [PlotStyle]
data Graph = Plots [Plot] [GraphStyle] | GBesides [Graph] | GAbove [Graph]  


--plotWindow [tms w,pts w]
           
t n = do w' <- loadWave $ "/home/tomn/waves/"++show n++"_ecVoltage.twv"
         let w = downSample 1000 w' --restrict w' 4 6)
         print $ npnts w
         let mrks = [(x,0) | x <- [1..6]]::[(Double,Double)]
         plotGraph $ w % blue <+> mrks % red % "some dots" 
         return ()

main = t 8017

plotGraph :: ToGraph g => g -> IO ()
plotGraph g = plotGraph' . toGraph $ g
-- plotGraph' (GBesides ps) = renderableToWindow (toRenderable $ layout) width height
plotGraph' (Plots plots styles) = renderableToWindow (toRenderable $ layout) width height
    where layout =  layout1_plots ^= (map plotToLayout plots) $ defaultLayout1
          height = [ h | Height h <- styles] `headOr` 480
          width = [ w | Width w <- styles] `headOr` 640
          plotToLayout (Plot pts pstys) = if Lines `elem` pstys
                                             then (leg, Left (C.toPlot lnPlot))
                                             else (leg, Left (C.toPlot ptPlot))
              where leg = [ l | Legend l <- pstys] `headOr` "a graph"
                    col = [ l | Col l <- pstys] `headOr` black
                    lnPlot = plot_lines_values ^= [pts] $ 
                         plot_lines_style  .> line_color ^= col
                         $ defaultPlotLines
                    ptPlot = plot_points_style ^= filledCircles 2 col
                             $ plot_points_values ^= pts
                             $ defaultPlotPoints
                   
                         

headOr [] def = def
headOr (x:_) _ = x

Plot x ys +^^ y = Plot x (y:ys)

class ToPlotStyle a where 
    toPlotStyle :: a -> PlotStyle

instance ToPlotStyle Color where
    toPlotStyle = Col

instance ToPlotStyle [Char] where
    toPlotStyle = Legend

instance ToPlotStyle PlotStyle where
    toPlotStyle = id

instance ToPlotStyle Double where
    toPlotStyle = LineWidth

class ToPlot a where
    toPlot :: a -> Plot

instance ToPlot Plot where
    toPlot = id

instance ToPlot [(Double,Double)] where
    toPlot p = Plot p [FilledCircles]


class ToGraph a where
    toGraph :: a -> Graph

instance ToGraph Graph where
    toGraph = id

instance ToPlot a => ToGraph a where
    toGraph p = Plots [toPlot p] []

(<+>) :: (ToGraph a, ToGraph b) => a-> b->Graph 
a <+> b = amal (toGraph a) (toGraph b)
    where amal (Plots pls1 prb1) (Plots pls2 prb2) = Plots (pls1++pls2) (prb1++prb2)

infixl 6 %
infixl 5 <+>
infixl 4 <|>
-- infixl 3 <->

(%) :: (ToPlot a, ToPlotStyle b) => a -> b -> Plot
p % s = addst (toPlot p) (toPlotStyle s)
        where addst (Plot pts s) s' = Plot pts (s':s)

(<|>) :: (ToGraph a, ToGraph b) => a-> b->Graph 
a <|> b = amal (toGraph a) (toGraph b)
    where amal p1@(Plots pls1 prb1) p2@(Plots pls2 prb2) = GBesides [p1,p2]
          amal p1@(Plots pls1 prb1) (GBesides p2) = GBesides (p1:p2)
          amal (GBesides p2) p1@(Plots pls2 prb2) = GBesides (p2++[p1])
