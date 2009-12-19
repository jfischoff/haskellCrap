import Mesh
import Test.QuickCheck
import Text.Printf
import Data.SG

main = do mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests  = [("prop_TriangleConstuct", test prop_TriangleConstuct)]



prop_TriangleConstuct = fst (True, Triangle (Point3 (1.0, 1.0, 1.0)) 
	(Point3 (1.0, 1.0, 1.0)) 
	(Point3 (1.0, 1.0, 1.0)))