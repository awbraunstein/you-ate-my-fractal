module Main where

import Draw
import Complex
import Fractal

maxIteration :: Int
maxIteration = 100

-- Pixel -> Pixel -> Iteration(color)
mandel :: Fractal
mandel x y = colorFromValue $ f (C r i) 0 maxIteration where
  r = 2.5 * x / (realToFrac width)
  i = 2.0 * y / (realToFrac height)
  f :: Complex -> Complex -> Int -> Int
  f _ _ 0 = 0
  f c z i
    | magnitude z > 2 = i
    | otherwise = f c ((z * z) + c) (i - 1)


c :: Complex
c = C (-0.423) 0.745

julia :: Fractal
julia x y = colorFromValue $ f c (C r i) 0 where
  r = 1.5 * x / (realToFrac width)
  i = 1.5 * y / (realToFrac height)
  f c z iter
    | iter > maxIteration = 0
    | otherwise = let z' = z^2 + c in
                  if magnitude(z') > 2
                  then iter
                  else f c z' (iter+1)

ship :: Fractal
ship x y = colorFromValue $ f (C r i) 0 0 where
  r = 2 * x / (realToFrac width)
  i = 2 * y / (realToFrac height)
  f c z iter
    | iter > maxIteration = 0
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

newton :: Fractal
newton x y = colorFromValue $ f (C r i) 0 where
  r = 2 * x / (realToFrac width)
  i = 2 * y / (realToFrac height)
  f z iter
    | iter > maxIteration = 0
    | ( abs $ magnitude l - magnitude z) > minDiff = f l (iter + 1)
    | otherwise = iter
    where l = z - it z / dx z

colorFromValue :: Colorize
colorFromValue n = Color (t n) (t (n+5)) (t (n+10))
  where
      t :: Int -> Float
      t i = 0.5 + 0.5*cos( fromIntegral i / 10 )

colorFromValue' :: Colorize
colorFromValue' x
    | x > maxIteration = Color 255 255 255
    | otherwise = Color (realToFrac x) (realToFrac x) (realToFrac x)

main :: IO ()
main = draw $ newton
