module Draw where

import Graphics.Rendering.OpenGL hiding (Color)
import Graphics.UI.GLUT hiding (Color)
import Fractal
import QTree


width :: GLfloat
width = 640

height :: GLfloat
height = 640

draw :: Fractal -> IO ()
draw frac = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "You ate my fractal"
  displayCallback $= display frac
  mainLoop

display frac = do
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix $ drawFractal frac
  swapBuffers

drawFractal frac =
  renderPrimitive Points $ do
    mapM_ drawPoint $ getPixelValues frac
  where
    drawPoint (x,y,c) = do
      color c
      vertex $ Vertex3 x y 0

getPixelValues :: Fractal -> [(GLfloat,GLfloat,Color3 GLfloat)]
getPixelValues frac =
  [ (x/width, y/height, toColor3 $ frac (realToFrac x) (realToFrac y)) |
                  x <- [-width..width], -- Chooses whole numbers
                  y <- [-height..height]] -- Change this part to use the QTree

toColor3 :: Color -> Color3 GLfloat
toColor3 (Color r g b) = Color3 (realToFrac r) (realToFrac g) (realToFrac b)
