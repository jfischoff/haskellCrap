module OpenGLRenderer
import Data.SG
import Mesh
import Graphics.Rendering.OpenGL hiding (Polygon, multMatrix)
import Graphics.UI.GLUT hiding (cursor, Polygon, Solid, multMatrix)
import qualified Graphics.Rendering.OpenGL as GL

--This class just renders meshes 
--It hides the low level rendering functions from the main program
--It takes a Mesh and renders it 
--It should probably take a camera too

render :: SimpleMesh -> IO ()
render vertices = 
	clear [ColorBuffer]
	renderPrimitive Triangles $ mapM_ (vertex3 -> vertex vertex3) vertices
	flush