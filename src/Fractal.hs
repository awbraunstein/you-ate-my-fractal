module Fractal where

data Color = Color !Float !Float !Float

type Fractal = Float -> Float -> Color

type Colorize = Int -> Color

-- class Fractal where
--   colorIterations :: Int -> Color
--   f               :: Coordinate -> Int