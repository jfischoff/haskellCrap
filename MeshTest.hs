import Mesh
import Test.QuickCheck
import Text.Printf
import Data.SG

main = do mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests  = [("prop_LineTriangleIntersection", test prop_LineTriangleIntersection)]



prop_LineTriangleIntersection = lineTriangleIntersection (Line3 )