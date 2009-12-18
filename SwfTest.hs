import Swf.Assembler
import Swf.Assembly
import Swf.Bin
import Swf.Tags
import Swf.Util
import Test.QuickCheck
import Text.Printf

main = do Prelude.mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests = [("nullTest", test nullTest) ]

nullTest = False





