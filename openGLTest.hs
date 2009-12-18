{-# OPTIONS_GHC -fglasgow-exts #-}
import Data.Array.Storable
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  mainLoop
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) myPoints
  flush
 
 main = do arr <- newArray (1,10) 37 :: IO (StorableArray Int Float)
           a <- readArray arr 1
           withStorableArray arr 
               (\ptr -> memset ptr 0 40)
           b <- readArray arr 1
           print (a,b)
		   
 foreign import ccall unsafe "string.h" 
     memset  :: Ptr a -> CInt -> CSize -> IO ()