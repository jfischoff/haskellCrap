module Vector3 where
import Data.Functor
	
main = do print $ vecAverage $ take 3 $ cycle [fromList $ take 3 [0.0 .. 10.0]]

vecSum xs = foldl (.+) (Trp 0 0 0) xs

vecAverage xs = (1.0 / (fromIntegral $ length xs)).* (vecSum xs)

distance v = sqrt $ v `dot` v 
	
type DoubleType = Double
		
data Triple = Trp DoubleType DoubleType DoubleType deriving 
	(Show, Read, Eq, Ord) 
	
toList (Trp x y z) = [x, y, z]
fromList xs = Trp (xs !! 0) (xs !! 1) (xs !! 2)

class Vector v where
  (.+) :: v -> v -> v
  (.*) :: DoubleType -> v -> v
  dot :: v -> v -> DoubleType
  cross :: v -> v -> v

instance Vector Triple where
  Trp x y z .+ Trp u v w = Trp (x+u) (y+v) (z+w)
  a .* Trp x y z = Trp (a*x) (a*y) (a*z)
  dot (Trp x y z) (Trp u v w) = x*u+y*v+z*w
  cross (Trp x y z) (Trp u v w) = Trp
      (y*w-v*z)
      (z*u-x*w)
      (x*v-y*u)
	  

	  


 