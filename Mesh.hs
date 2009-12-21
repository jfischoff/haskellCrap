module Mesh where
import Data.SG
--import Graphics.Rendering.OpenGL hiding (Polygon, multMatrix, Triangle)

type DoubleType = Double
type Point3 = Point3' DoubleType
type Line3 = Line3' DoubleType
type Ray = Line3

data Triangle = Triangle { t0 :: Point3, t1 :: Point3, t2 :: Point3}

newtype SimpleMesh = SimpleMesh [Triangle]

type TrilinearCoord = Point3

-- newtype Plane = Quad DoubleType

-- convertTriangleToPlane

--find the triangle and the point in trilinear coordinates
--this will use the plane line intersection algorithm

-- findIntersectingTriangle :: SimpleMesh -> (Triangle, TrilinearCoord)

lineTriangleIntersection :: Line3 -> Triangle -> (Maybe TrilinearCoord)
lineTriangleIntersection Line3(start dir) Triangle(p0 p1 p2) = tuv where
	--put columns in 
	transposedMatrix = fromMatrixComponents [[toList dir], [p0 - p1],
		[p2 - p0]]
	matrix = transpose transposedMatrix
	invertedMatrix = inverse matrix
	tuv = matrixMult invertedMatrix 
	
inverse :: [[Rational]] -> [[Rational]]
inverse mat = sweep ([], zipWith (++) mat unit) where
  unit = map (take (length mat)) $ iterate (0:) (1:[0,0..])
  sweep (xss,[]) = xss
  sweep (xss,yss) = sweep (xss' ++ [ws], filter (any (/= 0)) yss') where
    Just (x:xs) = find ((/= 0).head) yss
    ws = map (/ x) xs
    [xss',yss'] = map (map f) [xss,yss]
    f (y:ys) = zipWith (\d e -> e - d*y) ws ys
	
	
-- intRayWith (Ray start dir) (Plane (a,b,c,d)) = if (abs(part) < 10**(-9)) 
												-- then []  
												-- else [- (d + ((a,b,c) *. start) ) / part]
  -- where
		-- part = (a,b,c) *. dir

