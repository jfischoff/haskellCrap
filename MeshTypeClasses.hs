--vertex is has a collection of streams for now position 
--it is really a collection of a few things
--transformable streams i.e. position normals 
--interpolating streams i.e colors normals
--blind data i.e. skinning weights
--need to make a point 2 for uvs
data Vertex a = Vertex {pos :: Point a}

--I think that it should be a different type if it has a different column
--That way the type system will help me make sure that the functions work
--So I need a way to tack on streams
--I have a stream class and then I have a class that is a collection of stream
--A stream is just a list
--there are a bunch of type classes if the mesh responds to certain 
--functions
-- class StreamCollection a b where
	-- getTransfromable :: [Coord b]
	-- getInterpolatable :: []
	
class Positionable a where	
	getPos :: Coord b => a -> b
	
--class Tangentable a where 
--	getTangent :: Coord b => a -> b
	
--class Interpolatable a where 
	--getInterpolatable :: Num b => a -> [b]

--class Stridable a
	--this is a way to get the components out in a different orders
	--probably can pass in the functions for accessing the elements
	
--class Packable a
	--This is a way to take a stride and turn it into a binary 
	--blob. Need someway to say first x bits treat as x as do y or something
	
class (Positionable a) => Vertex a
	
--this a way for me to collect all the components that get transformed
--the tangents get normalized
--class Transformable a where 
	--need to do a matrix multiply. Should build it from the dot
	--product
	--transform :: 
	
class (Positionable a, Transformable a, Interpolatable a) => Mesh a where
	getVertex :: Int -> a
	getTriangleIndices :: [Int]
	--transform
