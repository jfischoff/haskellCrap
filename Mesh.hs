module Mesh where

newtype Point3D a = (a, a, a)

x (a, _, _) = a
y (_, b, _) = b
z (_, _, c) = c

data Triangle a = Triangle { t0 :: a, t1 :: a, t2 :: a}

data Mesh a = Mesh {verts :: [Point3D a], triangles :: [Triangle Int]}

newType SimpleMesh = [Triangle Point3D]

