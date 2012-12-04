module Draw where

import Graphics.Rendering.OpenGL hiding (Color, Q)
import Graphics.UI.GLUT hiding (Color, Q)
import Data.IORef
import Data.Maybe
import Fractal
import QTree


draw :: Fractal -> Range -> IO ()
draw frac rng = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "You ate my fractal"
  let tree = mkColorQTree frac rng
  res <- newIORef (3 :: Int)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse res)
  idleCallback $= Just idle
  displayCallback $= display tree rng res
  mainLoop


keyboardMouse res key state modifiers position = do
  keyboardAct res key state

keyboardAct res (Char 'e') Down = do
  res' <- get res
  res $= res' + 1
keyboardAct res (Char 'd') Down = do
  res' <- get res
  res $= max (res' - 1) 0
keyboardAct _ _ _ = return ()

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

display tree rng res = do
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix $ do
    res' <- get res
    drawFractal tree rng res'
  swapBuffers

idle = do
  postRedisplay Nothing

drawFractal :: QTree Color -> Range -> Int -> IO ()
drawFractal tree rng res =
  renderPrimitive Quads $ do
    getPixelValues tree rng res

drawPoint (x,y,c) = do
  color c
  vertex $ Vertex3 x y 0

toColor3 :: Color -> Color3 GLfloat
toColor3 (Color r g b) = Color3 (realToFrac r) (realToFrac g) (realToFrac b)

getPixelValues :: QTree Color -> Range -> Int -> IO ()
getPixelValues tree rng res = drawAtDepth res tree rng
                -- walkDown tree rng

-- Scale a point in a Range to be in the range -1 1 1 -1
scalePoint :: Range -> (Float, Float) -> (GLfloat, GLfloat)
scalePoint (Q _ y1 x2 _) (x,y) = (realToFrac (x / sf), realToFrac (y/sf))
  where sf = max y1 x2



  -- [ (x/width, y/height, toColor3 $ frac (realToFrac x) (realToFrac y)) |
  --                 x <- [-width..width], -- Chooses whole numbers
  --                 y <- [-height..height]] -- Change this part to use the QTree

drawQuad :: Range -> Color3 GLfloat -> Range -> IO ()
drawQuad (Q x1 y1 x2 y2) c rng = drawPoint (x1', y1', c) >>
                                 drawPoint (x2', y1', c) >>
                                 drawPoint (x2', y2', c) >>
                                 drawPoint (x1', y2', c)
  where (x1', y1') = scalePoint rng (x1, y1)
        (x2', y2') = scalePoint rng (x2, y2)


drawAtDepth :: Int -> QTree Color -> Range -> IO ()
drawAtDepth 0 t rng = drawQuad (fromJust $ range t) (toColor3 $ fromMaybe (white) (nodeVal t)) rng
drawAtDepth n t rng = (drawAtDepth (n-1) (fromJust $ ulC t) rng) >>
                      (drawAtDepth (n-1) (fromJust $ urC t) rng) >>
                      (drawAtDepth (n-1) (fromJust $ lrC t) rng) >>
                      (drawAtDepth (n-1) (fromJust $ llC t) rng)

walkDown :: QTree Color -> Range -> [(GLfloat, GLfloat, Color3 GLfloat)]
walkDown Empty _ = []
walkDown t rng =
  (walkRight t rng) ++ walkDown (fromMaybe Empty $ bottomN t) rng

walkRight :: QTree Color -> Range -> [(GLfloat, GLfloat, Color3 GLfloat)]
walkRight Empty _ = []
walkRight t rng=
  (x, y, (toColor3 $ fromMaybe (white) (nodeVal t))) : walkRight (fromMaybe Empty $ rightN t) rng
    where (x,y) = scalePoint rng (midpoint (fromJust $ range t))


getStart :: Int -> QTree Color -> QTree Color
getStart 0 t = t
getStart n t = getStart (n-1) (fromJust $ ulC t)


mkColorQTree :: Fractal -> Range -> QTree Color
mkColorQTree frac r = link (Q Empty Empty Empty Empty) r ROOT Nothing where
  link :: Quad (Neighbor Color) -> Range -> Child -> Maybe (QTree Color) -> QTree Color
  link ns rng ch p =
    let t = (Branch
             (frac mx my)
             (Q
               (link (fromJust $ neighbors t) (ulQ rng) UL (Just t))
               (link (fromJust $ neighbors t) (urQ rng) UR (Just t))
               (link (fromJust $ neighbors t) (lrQ rng) LR (Just t))
               (link (fromJust $ neighbors t) (llQ rng) LL (Just t))
             )
             (linkN ch p)
             rng
            ) in t
      where (mx , my) = midpoint rng
            linkN :: Child -> Maybe (QTree Color) -> Quad (Neighbor Color)
            linkN ROOT _ = Q Empty Empty Empty Empty
            linkN _ Nothing = Q Empty Empty Empty Empty
            linkN UL (Just t) = (Q
                          (fromMaybe Empty $ llC $ fromMaybe Empty (topN t)) -- top
                          (fromMaybe Empty $ ulC $ fromMaybe Empty (rightN t)) -- left
                          (fromMaybe Empty $ urC t) -- right
                          (fromMaybe Empty $ llC t) -- bottom
                         )
            linkN UR (Just t) = (Q
                          (fromMaybe Empty $ lrC $ fromMaybe Empty (topN t))
                          (fromMaybe Empty $ ulC t)
                          (fromMaybe Empty $ ulC $ fromMaybe Empty (rightN t))
                          (fromMaybe Empty $ lrC t)
                         )
            linkN LR (Just t) = (Q
                          (fromMaybe Empty $ urC t)
                          (fromMaybe Empty $ llC t)
                          (fromMaybe Empty $ llC $ fromMaybe Empty (rightN t))
                          (fromMaybe Empty $ urC $ fromMaybe Empty (bottomN t))
                         )
            linkN LL (Just t) = (Q
                          (fromMaybe Empty $ ulC t)
                          (fromMaybe Empty $ lrC $ fromMaybe Empty (leftN t))
                          (fromMaybe Empty $ lrC t)
                          (fromMaybe Empty $ ulC $ fromMaybe Empty (bottomN t))
                         )
