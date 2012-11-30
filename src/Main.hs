module Main where

import Draw
import Complex
import Fractal

maxIteration :: Int
maxIteration = 1000

-- Pixel -> Pixel -> Iteration(color)
mandel :: Fractal
mandel x y = f (C r i) 0 maxIteration where
  r = 2.5 * x / (realToFrac width)
  i = 2.0 * y / (realToFrac height)
  f :: Complex -> Complex -> Int -> Int
  f _ _ 0 = 0
  f c z i
    | magnitude z > 2 = i
    | otherwise = f c ((z * z) + c) (i - 1)

main :: IO ()
main = draw mandel
