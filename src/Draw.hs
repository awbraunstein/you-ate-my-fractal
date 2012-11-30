
module Draw where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Fractal
import QTree


width :: GLfloat
width = 320

height :: GLfloat
height = 320

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
  [ (x/width, y/height, colorFromValue $ frac (realToFrac x) (realToFrac y)) |
                  x <- [-width..width], -- Chooses whole numbers
                  y <- [-height..height]] -- Change this part to use the QTree

colorFromValue :: Int -> Color3 GLfloat
colorFromValue n = Color3 (t n) (t (n+5)) (t (n+10))
  where
      t :: Int -> GLfloat
      t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
