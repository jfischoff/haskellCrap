module Huffman where
import Control.Arrow
import Data.ByteString.Lazy
import Data.Binary.Put
import Data.Binary.BitPut
import Data.Bits
import Data.Word
import Test.QuickCheck
import Text.Printf
import List


main = do
	print $ entropy [0.5, 0.5]
	--mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
	
tests = [("prop_BitString", test prop_BitString),
	("prop_FreqTest", test prop_FreqTest),
	("prop_EntropyTest", test prop_EntropyTest)]
	
prop_BitString = ("1010101010101010" == (printByteStringAsBits $ runBitPut buildBitString))
prop_Fail = False

prop_FreqTest = ([('a', 1), ('b', 2), ('c', 3)] == (computeFreqs ['a', 'b', 'b', 'c', 'c', 'c']))

prop_EntropyTest = (0.5 == (entropyOfFreqs [1, 1]))

buildBitString :: BitPut 
buildBitString = do
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	putBit True
	putBit False
	
-- I need to write bits
-- I need a way to print a byte string a 
printByteStringAsBits :: ByteString -> String
printByteStringAsBits byteString = Prelude.foldl (++) [] $ Data.ByteString.Lazy.zipWith word8ToString byteString byteString

word8ToString :: Word8 -> Word8 -> String
word8ToString word8 dummy = bitStringToZerosAndOnes $ word8ToBitString word8 dummy

word8ToBitString :: Word8 -> Word8 -> [Bool]
word8ToBitString word8 dummy = [testBit word8 7, testBit word8 6,
	testBit word8 5, testBit word8 4, testBit word8 3, testBit word8 2,
	testBit word8 1, testBit word8 0]
	
bitStringToZerosAndOnes :: [Bool] -> String
bitStringToZerosAndOnes bitString = Prelude.map bitToString bitString

bitToString :: Bool -> Char
bitToString boolean = if boolean == True then
		'1'
	else
		'0'
		
computeFreqs :: Ord a => [a] -> [(a, Int)]
computeFreqs list = freqsAndGroups where
	groups = List.group (List.sort list)
	groupIds = Prelude.map List.head groups
	freqs = Prelude.map Prelude.length (groups)
	freqsAndGroups = Prelude.zip groupIds freqs
	
entropyOfFreqs :: [Int] -> Double
entropyOfFreqs freqs = entropy $ probsFromFreqs freqs

entropy probs = (-1.0) * (sum $ List.zipWith (*) probs $ Prelude.map log probs)
	
adjustList doubleList = List.map (1.0 + ) doubleList

probsFromFreqs freqs = probs where
	doubleList = intListToDoubleList freqs
	adjustedList = adjustList doubleList
	totalCount = (sum doubleList) + (fromIntegral $ Prelude.length adjustedList)
	scaler = recip totalCount
	probs = List.map (scaler *) adjustedList
	
intListToDoubleList :: [Int] -> [Double]
intListToDoubleList list = 	List.map fromIntegral list