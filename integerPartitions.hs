import Math.Combinat.Partitions
import List

--main = do print $ map (\x -> multiplicity (fst x)) $ map freqOfPart $ dicePartitions 4 12
main = do print $ percentMultParts $ constrainedRoll 5
--main = do print $ toFirst multiplicity ([4, [1])

percentMultParts multParts = pertParts where
	total = fst $ foldl (\x y -> ((fst x) + (fst y), (snd y))) (0, []) multParts
	pertParts = map (\x -> ((fromIntegral (fst x)) / (fromIntegral total), snd x)) multParts

constrainedRoll rollcount = sortedParts rollcount (rollcount * 3)
sortedParts rollCount total = sortBy multPartCompare $ map multOfPart $ (dicePartitions rollCount total)

dicePartitions :: Int -> Int -> [Partition]
dicePartitions rollCount total = restrictedPartitions where
	parts = partitions' (6, rollCount) total
	restrictedPartitions = filter 
		(\x -> (length $ fromPartition x) == rollCount) parts
		
multOfPart part = toFirst multiplicity $ freqOfPart part

multPartCompare x y | (fst x) > (fst y) = LT
					| (fst x) == (fst y) = EQ
					| (fst x) < (fst y) = GT
		
freqOfPart part = foldl (\x y -> ((fst x)++[fst y], (snd x) ++ (snd y))) ([], []) $
	map (\x -> (length x, x) ) $ group $ fromPartition part

toFirst f (a, b) = (f a, b)
		
multiplicity :: [Int] -> Int
multiplicity elements = mult where
	totalFac = fac $ sum elements
	bottom = product $ map fac elements
	mult = totalFac `div` bottom
	
fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x - 1)