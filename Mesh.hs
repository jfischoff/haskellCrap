module Mesh where
import Data.SG
import Graphics.Rendering.OpenGL hiding (Polygon, multMatrix, Triangle)

type Point3 = Point3' GLdouble

data Triangle = Triangle { t0 :: Point3, t1 :: Point3, t2 :: Point3}

newtype SimpleMesh = SimpleMesh [Triangle]



