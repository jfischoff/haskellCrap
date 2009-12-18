--this rewrites list functions that use a file instead of a list in memory

--recursively read call function
--opens the file
--the type is derived from binary
--MutableFileList a = MutableFileList {mflType :: a, mflCount :: Int}
--make a binary instance that only writes out the count

--Read from a byteString
--error if past count
--mflRead :: MutableFileList a -> Int -> ByteString -> a

--mflWrite :: MutableFileList a -> Int -> ByteString -> ByteString

--mflMap :: IO () -> MutableFileList a -> IO ()

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr outh result
       hClose inh
       hClose outh

processData :: String -> String
processData = map toUpper