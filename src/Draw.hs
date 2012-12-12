module Draw where

import Graphics.Rendering.OpenGL hiding (Color, Q)
import Graphics.UI.GLUT hiding (Color, Q)
import Data.IORef
import Data.Maybe
import Fractal
import QTree


draw :: Fractal -> Range -> IO ()
draw frac rngi = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "You ate my fractal"
  let tree = mkColorQTree frac rngi
  rng' <- newIORef (rngi :: Range)
  rangeStart <- newIORef ((0.0,0.0) :: (Float, Float))
  res <- newIORef (3 :: Int)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse res rangeStart rng' rngi)
  idleCallback $= Just idle
  displayCallback $= display tree rng' res
  mainLoop


keyboardMouse res rangeStart rng rngi key state modifiers position = do
  keyboardAct rng rngi res key state
  mouseAct rangeStart rng position key state

mouseAct rangeStart _ (Position x y) (MouseButton LeftButton) Down = do
  (Size x' y') <- get windowSize
  let x'' = (fromIntegral x) / (fromIntegral x')
  let y'' = (fromIntegral y) / (fromIntegral y')
  rangeStart $= (x'', y'')

mouseAct rangeStart rng (Position x y) (MouseButton LeftButton) Up = do
  (Size x' y') <- get windowSize
  let x'' = (fromIntegral x) / (fromIntegral x')
  let y'' = (fromIntegral y) / (fromIntegral y')
  (sx, sy) <- get rangeStart
  (Q x1 y1 x2 y2) <- get rng
  if x'' > sx && y'' > sy then
    do
      let dx = x2 - x1 -- this code is slightly fucked
      let dy = y1 - y2
      let sx' = (sx * dx) + x1
      let x''' = (x'' * dx) + x1
      let sy' = ((1 - sy) * dy) + y2
      let y''' = ((1 - y'') * dy) + y2
      let newRng = (Q sx' sy' x''' y''')
      rng $= newRng
    else
      return ()
mouseAct _ _ _ _ _ = return ()

keyboardAct _ _ res (Char 'e') Down = do
  res' <- get res
  res $= res' + 1
keyboardAct _ _ res (Char 'd') Down = do
  res' <- get res
  res $= max (res' - 1) 0
keyboardAct rng rngi res (Char 'r') Down = do
  rng $= rngi
  res $= 3
-- Pan Up
keyboardAct rng (Q _ y1i _ y2i) _ (SpecialKey KeyUp) Down = do
  (Q x1 y1 x2 y2) <- get rng
  let ydif = y1 - y2
  rng $= (Q x1 (min y1i (y1 + (0.2 * ydif))) x2 (min (y1i - ydif) (y2 + (0.2 * ydif))))
-- Pan Down
keyboardAct rng (Q _ y1i _ y2i) _ (SpecialKey KeyDown) Down = do
  (Q x1 y1 x2 y2) <- get rng
  let ydif = y1 - y2
  rng $= (Q x1 (max (y2i + ydif) (y1 - (0.2 * ydif))) x2 (max y2i (y2 - (0.2 * ydif))))
-- Pan Left
keyboardAct rng (Q x1i _ x2i _) _ (SpecialKey KeyLeft) Down = do
  (Q x1 y1 x2 y2) <- get rng
  let xdif = x2 - x1
  rng $= (Q (max x1i (x1 - (0.2 * xdif))) y1 (max (x1i + xdif) (x2 - (0.2 * xdif))) y2)
-- Pan Right
keyboardAct rng (Q x1i _ x2i _) _ (SpecialKey KeyRight) Down = do
  (Q x1 y1 x2 y2) <- get rng
  let xdif = x2 - x1
  rng $= (Q (min (x2i - xdif) (x1 + (0.2 * xdif))) y1 (min x2i (x2 + (0.2 * xdif))) y2)

keyboardAct _ _ _ _ _ = return ()

reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

display tree rng res = do
  clear [ColorBuffer]
  loadIdentity
  preservingMatrix $ do
    res' <- get res
    rng' <- get rng
    drawFractal tree rng' res'
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
                -- getStart tree
                -- walkDown tree rng

-- Scale a point in a Range to be in the range -1 1 1 -1
scalePoint :: Range -> (Float, Float) -> (GLfloat, GLfloat)
scalePoint (Q x1 y1 x2 y2) (x,y) = (realToFrac ((normx * 2) - 1), realToFrac ((normy * 2) - 1))
  where xdist = x2 - x1
        ydist = y1 - y2
        normx = (x - x1) / xdist
        normy = (y - y2) / ydist



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
drawAtDepth n t rng =
  (if intersect (fromJust $ range (fromJust $ ulC t)) rng then
     (drawAtDepth (n-1) (fromJust $ ulC t) rng)
   else return ()) >>
  (if intersect (fromJust $ range (fromJust $ urC t)) rng then
     (drawAtDepth (n-1) (fromJust $ urC t) rng)
   else return ()) >>
  (if intersect (fromJust $ range (fromJust $ lrC t)) rng then
     (drawAtDepth (n-1) (fromJust $ lrC t) rng)
   else return ()) >>
  (if intersect (fromJust $ range (fromJust $ llC t)) rng then
     (drawAtDepth (n-1) (fromJust $ llC t) rng)
   else return ())

walkDown :: QTree Color -> Range -> IO ()
walkDown Empty _ = return ()
walkDown t rng =
  if intersect (fromJust $ range t) rng then
    ((walkRight t rng) >> walkDown (fromMaybe Empty $ bottomN t) rng)
  else return ()

walkRight :: QTree Color -> Range -> IO ()
walkRight Empty _ = return ()
walkRight t rng=
          if intersect (fromJust $ range t) rng then
            (drawQuad (fromJust $ range t) (toColor3 $ fromMaybe (white) (nodeVal t)) rng
                       >>
            walkRight (fromMaybe Empty $ rightN t) rng)
          else return ()
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
