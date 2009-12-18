module Geometry where
import Data.Traversable
import Control.Applicative
import Data.Foldable
import Test.QuickCheck
import Text.Printf
import Control.Monad
import List


main = do
	print $ fmap (1+) (Triangle (Point 1.0 0.0 0.0) (Point 1.0 0.0 0.0) (Point 1.0 0.0 0.0))
	--Prelude.mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
	
tests = [("prop_CircumCircle", test prop_CircumCircle) ]

--prop_lsFromStartEnd 
--prop_extendLine 
--prop_ptDistance 


prop_CircumCircle :: Bool
prop_CircumCircle = result where 
	circle0 = Circle (Point 0.0 0.0 1.5) 0.5
	circle1 = circumCircle [(Point 0.0 0.0 1.0), (Point 0.0 0.0 2.0)]
	result = circle0 == circle1
	
-- Minimal implementation: fromComponents
class (Traversable p, Functor p, Foldable p, Applicative p) => Coord p where
  -- | Gets the components of the vector, in the order x, y (, z).
  getComponents :: Num a => p a -> [a]
  getComponents = toList
  -- | Re-constructs a vector from the list of coordinates.  If there are too few,
  -- the rest will be filled with zeroes.  If there are too many, the latter ones are
  -- ignored.
  fromComponents :: Num a => [a] -> p a
  -- | Gets the magnitude squared of the vector.  This should be fast for
  -- repeated calls on 'Data.SG.Geometry.TwoDim.Rel2'' and
  -- 'Data.SG.Geometry.ThreeDim.Rel3'', which cache this value.
  magSq :: Num a => p a -> a
  magSq = Prelude.sum . Prelude.map (\x -> x * x) . getComponents

  -- | Computes the dot product of the two vectors.
  dotProduct :: Num a => p a -> p a -> a
  dotProduct a b = Prelude.sum $ zipWith (*) (getComponents a) (getComponents b)
  

-- I should 
  
data Point a = Point {px :: a, py :: a, pz :: a}
	deriving (Eq, Ord, Show, Read)
	
instance Applicative Point where
	pure a = (Point a a a)
	(<*>) (Point fa fb fc) (Point a b c) = Point (fa a) (fb b) (fc c)
instance Foldable Point where
	foldMap = foldMapDefault
instance Functor Point where 
	fmap = fmapDefault
instance Traversable Point where
	traverse f (Point x y z) = Point <$> f x <*> f y <*> f z
--the 'a' type has to have these things
instance (Show a, Eq a, Num a) => Num (Point a) where
	(+) = liftA2 (+)
	(-) = liftA2 (-)
	(*) = liftA2 (*)
	abs = fmap abs
	signum = fmap signum
	negate = fmap negate
	fromInteger = pure . fromInteger

instance Coord Point where 
	fromComponents list = Point (list !! 0) (list !! 1) (list !! 2) 
	
instance Arbitrary a => Arbitrary (Point a) where 
	arbitrary = do 
		x <- arbitrary
		y <- arbitrary
		z <- arbitrary
		return (Point x y z)
	coarbitrary = undefined
	
data Triangle a  = Triangle {t0 :: (Point a), t1 ::  (Point a), t2 ::  (Point a)} deriving (Show, Eq)

instance Foldable Triangle where
	foldMap = foldMapDefault
instance Functor Triangle where 
	fmap = fmapDefault
instance Traversable Triangle where
	traverse f (Triangle x y z) = Triangle <$> traverse f x <*> traverse f y <*> traverse f z

data Circle a = Circle {cOrigin :: Point a, cRadius :: a} deriving (Show, Eq)
data Ray a = Ray {rOrigin :: Point a, rDirection :: Point a} deriving (Show, Eq)
data LineSegment a = LineSegment {lRay :: Ray a, lDistance :: a} deriving (Show, Eq)
data Polygon a = Polygon {pPoints :: [a]} deriving (Show, Eq)

lsFromStartEnd :: (Floating a) => Point a -> Point a -> (LineSegment a)
lsFromStartEnd start end = ls where
	ray = Ray start (start)
	ptDis = ptDistance start end
	ls = LineSegment ray (px start)

extendLine :: Floating a => a -> (LineSegment a) -> Point a
extendLine u line = point where
	origin = rOrigin $ lRay line
	dist = u * lDistance line
	diffVect = (Point dist dist dist) * (rDirection $ lRay line)
	point = origin + diffVect

ptDistance :: Floating a => Point a -> Point a -> a
ptDistance start end = dis where
	dif = end - start
	sqrDif = dotProduct dif dif
	dis = sqrt sqrDif
	
delaunay :: [Point a] -> [Triangle a]
delaunay points = [Triangle (points !! 0) (points !! 0) (points !! 0)] 
	

circumCircle :: (Ord a, Floating a) => [Point a] -> Circle a
circumCircle points = circle where
	allLines = [ lsFromStartEnd start end | start <- points, end <- points, start /= end ]
	distances = map lDistance allLines
	disAndLines = zip distances allLines
	maxLine = head $ sortBy (\x y -> (fst x) `compare` (fst y)) disAndLines
	radius = 0.5 * (fst maxLine)
	origin = extendLine 0.5 (snd maxLine)		
	circle = Circle origin radius 

triangleCircumCircles :: (Ord a, Floating a) => Triangle (Point a) -> Circle a
triangleCircumCircles triangle = circumCircle $ toList triangle

inscribedPointCount :: [Point a] -> Circle a -> Int
inscribedPointCount points circle = -1



