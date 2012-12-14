module Fractals where

import Complex
import Fractal
import QTree

defaultMaxIteration :: Int
defaultMaxIteration = 200

defaultRange :: Range
defaultRange = Q (-2) 2 2 (-2)

-- Pixel -> Pixel -> MaxIteration -> Iteration(color)
mandel :: ItFractal
mandel mi x y = colorFromValue $ f (C x y) 0 mi where
  f :: Complex -> Complex -> Int -> Int
  f _ _ 0 = 0
  f c z i
    | magnitude z > 2 = i
    | otherwise = f c ((z * z) + c) (i - 1)


c :: Complex
c = C (-0.423) 0.745

julia :: ItFractal
julia mi x y = colorFromValue $ f c (C x y) 0 where
  f c z iter
    | iter > mi = 0
    | otherwise = let z' = z^2 + c in
                  if magnitude(z') > 2
                  then iter
                  else f c z' (iter+1)

ship :: ItFractal
ship mi x y = colorFromValue $ f (C x y) 0 0 where
  f c z iter
    | iter > mi = 0
    | otherwise = let z' = (C (magnitude z) (abs(im z)))^2 + c in
                  if magnitude z' > 2
                  then iter
                  else f c z' (iter+1)

-- How close successive tests must be before we declare they have converged
minDiff = 0.0001

-- formula to iterate
it z = 3 * z^4 - 1
-- derivative
dx z = 12 * ( z^3)

newton :: ItFractal
newton mi x y = colorFromValue $ f (C x y) 0 where
  f z iter
    | iter > mi = 0
    | ( abs $ magnitude l - magnitude z) > minDiff = f l (iter + 1)
    | otherwise = iter
    where l = z - it z / dx z

test :: ItFractal
test _ x y
  | x <= 0 && y >= 0 = Color 1 0 1
  | x <= 0 && y < 0 = Color 0 1 0
  | x > 0 && y >= 0 = Color 0 0 1
  | x > 0 && y < 0 = Color 1 0 0

colorFromValue :: Colorize
colorFromValue n = Color (t n) (t (n+5)) (t (n+10))
  where
      t :: Int -> Float
      t i = 0.5 + 0.5*cos( fromIntegral i / 10 )

colorFromValue' :: Colorize
colorFromValue' x
    | x > defaultMaxIteration = Color 255 255 255
    | otherwise = Color (realToFrac x) (realToFrac x) (realToFrac x)

-- mandel Q (-1.2579957) 0.9356985 (-0.5415778) 0.11086464