import Math.Combinat.Partitions
import List

main = do print $ map (\x -> multiplicity (fst x)) $ map freqOfPart $ dicePartitions 4 12
--main = do print $ freqOfPart $ (dicePartitions 4 12) !! 0
--main = do print $ freqOfPart $ (dicePartitions 4 12) !! 0
dicePartitions :: Int -> Int -> [Partition]
dicePartitions rollCount total = restrictedPartitions where
	parts = partitions' (6, rollCount) total
	restrictedPartitions = filter 
		(\x -> (length $ fromPartition x) == rollCount) parts
		
freqOfPart part = 
freqOfPart part = map (\x -> (length x, x) ) $ group $ fromPartition
		
multiplicity :: [Int] -> Int
multiplicity elements = mult where
	totalFac = fac $ sum elements
	bottom = product $ map fac elements
	mult = totalFac `div` bottom
	
fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x - 1)