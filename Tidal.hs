module Tidal where
import PopupGraph
import IO
import Monad
import Test.QuickCheck
import Text.Printf
import List
import Compression

main = do
	--let returnValue = (parseLines ["001POHNPEI 1975 12   779  789  784  775  762  759  779  764  748  746 9999", "001POHNPEI 1975 12   779  789  784  775  762  759  779  764  748  746 9999"] )
	--print returnValue
	--mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
	tideSeries
	
tests = [("prop_TestParseLine", test prop_TestParseLine),
	("prop_TestParseLines", test prop_TestParseLines)]

prop_TestParseLine =  ([753, 744, 749, 761, 725, 743, 779, 754, 759, 781, 788]== parseLine "001POHNPEI 1975 11   753  744  749  761  725  743  779  754  759  781  788")

prop_TestParseLines = ([779, 789, 784, 775, 762, 759, 779, 764, 748, 746, 9999]== parseLines ["001POHNPEI 1975 11   753  744  749  761  725  743  779  754  759  781  788", 
	"001POHNPEI 1975 12   779  789  784  775  762  759  779  764  748  746 9999"] )
	--let series = PopupGraph.createGraphData
	--PopupGraph.popupGraph series
	
getLines = liftM lines . readFile	

removeInvalid list = filter (/= 9999) list

parseLines :: [String] -> [Int]
parseLines list = tidalValues where
	noHeaderList = tail list
	flattened = concatMap parseLine noHeaderList
	tidalValues = removeInvalid flattened
	
parseLine :: String -> [Int]
parseLine line = intValues where
	splitup = words line
	values = drop 3 splitup
	intValues = map read values

makeOrderedPairs values = zip (intListToDoubleList $ createXValues values) (intListToDoubleList $ values)

tideSeries = do 
	list <- getLines "tidalData/d001.dat"
	let parsed = parseLines list	
	

	let orderedPairs = makeOrderedPairs parsed
	
	let slopes = floatList (-1.5) 1.5 10
	--let entropies = computeEntropyListOfLA parsed slopes
	--let slope = findMinSlopeGlobal parsed 100
	--print entropies
	
	PopupGraph.popupGraph [orderedPairs, [(0, 0), (1000, 1000 * 1)]]
	return Nothing
	

