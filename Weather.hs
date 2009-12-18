{-# LANGUAGE CPP #-}
{-# OPTIONS_DERIVE --output=file.h #-}
import Data.Binary
import Test.QuickCheck

import Date.DateTime

data WeatherRecordType = TMAX | TMIN | PREC | SNOW

type StationId = Int

data WeatherRecord a = WeatherRecord {wrType :: WeatherRecordType, value :: a, 
	date :: DateTime, stationId :: StationId}
	--deriving ({-! Arbitrary, Binary, Traversable !-})
	
data WeatherSeries a = WeatherSeries {wsType :: WeatherRecordType, 
	stationId :: StationId, startDate :: DateTime, timeInterval :: Int, 
	values :: [a]}

class BigSeries a b where
	readHeader = 
	writeHeader = 
	
	getValues = drop the size of the header
	
	mapValues :: ByteString -> (b -> c) -> a -> [c]
	

	
