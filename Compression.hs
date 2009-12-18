module Compression where
import Test.QuickCheck
import Text.Printf
import List

--main = do
	--print $ computeFreqsOfLine (-1.5) [0, 1, 2, 3] 
	--mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
	
tests = [("prop_FreqTest", test prop_FreqTest),
	("prop_EntropyTest", test prop_EntropyTest),
	("prop_Average", test prop_Average),
	("prop_SubtractAverage", test prop_SubtractAverage),
	("prop_CreateLineValues", test prop_CreateLineValues),
	("prop_SubtractLine", test prop_SubtractLine),
	("prop_ComputeEntropyOfLineAdjusted", test prop_ComputeEntropyOfLineAdjusted),
	("prop_FloatList", test prop_FloatList),
	("prop_FindMinSlope", test prop_FindMinSlope),
	("prop_ComputeIncrement", test prop_ComputeIncrement),
	("prop_FindMinSlope", test prop_FindMinSlope),
	("prop_FindMinSlopeGlobal", test prop_FindMinSlopeGlobal),
	("prop_ComputeFreqsOfLine", test prop_ComputeFreqsOfLine)
	]
	
	--need more tests
	

prop_FreqTest = ([('a', 1), ('b', 2), ('c', 3)] == (computeFreqs ['a', 'b', 'b', 'c', 'c', 'c']))
prop_EntropyTest = (0.6931471805599453 == (entropyOfFreqs [1, 1]))
prop_Average = (1.0 == (average [1.0, 2.0, 0.0, 1.0]))
prop_SubtractAverage = ([0.0, 1.0, -1.0, 0.0] == (subtractAverage [1.0, 2.0, 0.0, 1.0]))
prop_CreateLineValues = ([0.0, 2.0, 4.0, 6.0] == (createLineValues 2.0 [0, 1, 2, 3]) )
prop_SubtractLine = ([0, 0, 0, 0] == (subtractLine 2.0 [0, 2, 4, 6]))
prop_ComputeEntropyOfLineAdjusted = (0 == 
	(computeEntropyOfLineAdjusted 1.0 [0, 1]))
prop_FloatList = ([0.0, 0.1, 0.2, 0.3] == (floatList 0.0 0.4 4))
--start here
prop_FindMinSlope = False
prop_ComputeIncrement = ((computeIncrement (-1.0) 1.0 10 )== 0.2)
prop_FindMinSlopeGlobal = False
prop_ComputeFreqsOfLine = ([(0, 2)] == (computeFreqsOfLine 1.0 [0, 1]))

findMinSlopeGlobal list count = minSlope where
	angles = floatList (-1.5) 1.5 count
	slopes = map convertAngleToSlope angles
	minSlope = findMinSlope list slopes
	
convertAngleToSlope angle = tan angle
	
computeIncrement :: Double -> Double -> Int -> Double
computeIncrement start end count = increment where	
	dist = end - start
	increment = dist * (recip $ fromIntegral count)
	
findMinSlope :: [Int] -> [Double] -> Double
findMinSlope list slopes = minSlope where
	entropyList = computeEntropyListOfLA list slopes
	slopesAndEntropy = zip entropyList slopes
	minSlope = snd $ minimum slopesAndEntropy

computeEntropyListOfLA :: [Int] -> [Double] -> [Double]	
computeEntropyListOfLA list slopes = [
	computeEntropyOfLineAdjusted b list| b <- slopes]

floatList :: Double -> Double -> Int -> [Double]
floatList start end count = list where
	diff = end - start
	increment = computeIncrement start end count
	range = [0..(count - 1)]
	doubleList = intListToDoubleList range
	scaled = map (increment *) doubleList
	list = map (start +) scaled


computeFreqsOfLine slope list = computeFreqs $ subtractLine slope list

computeEntropyOfLineAdjusted :: Double -> [Int] -> Double	
computeEntropyOfLineAdjusted slope list = entropyOfFreqs 
	$ snd 
	$ unzip 
	$ computeFreqsOfLine slope list

subtractLine :: Double -> [Int] -> [Int]
subtractLine slope list = intAdjustValues where 
	doubleList = intListToDoubleList list
	lineValues = createLineValues slope $ createXValues list
	adjustValues = zipWith (-) lineValues doubleList
	intAdjustValues = map floor adjustValues

createLineValues :: Double -> [Int] -> [Double]
createLineValues slope xValues = lineValues where
	floatXValues = intListToDoubleList xValues
	lineValues = map (slope *) floatXValues
	
createXValues :: [Int] -> [Int]
createXValues list = [0..length list]

subtractAverage :: [Double] -> [Double]
subtractAverage list = adjustedValues where
	averageValue = (average list) * (-1.0)
	adjustedValues = map (averageValue +) list

average :: [Double] -> Double
average list = (sum list) / (fromIntegral $ length list)

computeFreqs :: Ord a => [a] -> [(a, Int)]
computeFreqs list = freqsAndGroups where
	groups = group (sort list)
	groupIds = map head groups
	freqs = map length (groups)
	freqsAndGroups = zip groupIds freqs
	
entropyOfFreqs :: [Int] -> Double
entropyOfFreqs freqs = entropy $ probsFromFreqs freqs

entropy probs = (-1.0) * (sum $ zipWith (*) probs $ map log probs)
	
adjustList doubleList = map (1.0 + ) doubleList

probsFromFreqs freqs = probs where
	doubleList = intListToDoubleList freqs
	adjustedList = adjustList doubleList
	totalCount = (sum doubleList) + (fromIntegral $ length adjustedList)
	scaler = recip totalCount
	probs = map (scaler *) adjustedList
	
intListToDoubleList :: [Int] -> [Double]
intListToDoubleList list = 	map fromIntegral list