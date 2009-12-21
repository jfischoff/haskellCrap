module Rasterizer
import Mesh
import Data.SG.Matrix
import Data.Colour

--this is the class that takes a 3d scene and turns it into a image
data Image = Image {pixels :: [[Colour]]}

type TransformationMatrix = Matrix44' Double

data Camera = Camera {cMatrix :: TransformationMatrix}

data Scene = Scene {meshes :: [SimpleMesh], camera :: Camera}

renderer :: Scene -> Int -> Int -> Image
renderer scene height width = image where 
	--project everything
	--convert to screen coordinates
	--for every pixel do create rays at the corners
	--
	
-- Send out a ray to intersect with the triangles
-- get back the interpolated vertex values 	
--castRay ::

-- 