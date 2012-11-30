module Mandel where

import Draw
import Complex
import Fractal

maxIteration :: Int
maxIteration = 1000

-- Pixel -> Pixel -> Iteration(color)
mandel :: Fractal
mandel x y = f (C x y) (C 0 0) 0 where
  f :: Complex -> Complex -> Int -> Int
  f c z i
    | (dot z) < 4 && i < maxIteration =
      f ((z * z) + c) z (i+1)
    | otherwise = i

main :: IO ()
main = draw mandel