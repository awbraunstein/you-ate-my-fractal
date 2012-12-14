module Fractal where

data Color = Color !Float !Float !Float
             deriving (Show)

type ItFractal = Int -> Fractal
type Fractal = Float -> Float -> Color

type Colorize = Int -> Color

white :: Color
white = Color 1 1 1

-- class Fractal where
--   colorIterations :: Int -> Color
--   f               :: Coordinate -> Int