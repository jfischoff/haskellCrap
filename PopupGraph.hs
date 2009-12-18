module PopupGraph where
import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Test.QuickCheck
import Text.Printf

--main = do
	--let series = createGraphData
	--popupGraph series
		--mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
		
--tests = [("prop_GraphTest", test prop_GraphTest)]

am :: Double -> Double
am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

createGraphData input = values where
		
	values = ([input], input)
	

createPointAndLinePlot title values = pointAndLinePlot 
	where	
		sinusoid1 = plot_lines_values ^= fst values
				  $ plot_lines_style  .> line_color ^= blue
				  $ plot_lines_title ^= title
				  $ defaultPlotLines

		sinusoid2 = plot_points_style ^= filledCircles 1 red
				  $ plot_points_values ^= snd values
				  $ plot_points_title ^= title
				  $ defaultPlotPoints
			
		pointAndLinePlot = (sinusoid2, sinusoid1)
		
makePlot input = plots where
	series = createGraphData input
	pointAndLine = createPointAndLinePlot "am" series

	plots = [Left (toPlot $ snd pointAndLine), Left (toPlot $ fst pointAndLine)]

chart title inputs = layout
	where
		plots = foldl (++) [] $ map makePlot inputs
		layout = layout1_title ^= title
				$ layout1_plots ^= plots
				$ defaultLayout1
				
popupGraph inputs = renderableToWindow (toRenderable $ chart "" inputs) 640 480

