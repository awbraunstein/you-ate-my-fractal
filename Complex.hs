-- Lightweight Complex Number Library

module Complex where

data Complex = C {-# UNPACK #-} !Float {-# UNPACK #-} !Float
               deriving (Show,Eq)

instance Num Complex where
    fromInteger n     = C (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t) (y*z + x*t)
    (C x y) + (C z t) = C (x+z) ( y+t)
    abs (C x y)       = C (sqrt (x*x + y*y)) 0.0
    signum (C x y)    = C (signum x) 0.0

-- Probably unnecessary
complex :: Float -> Float -> Complex
complex = C

real :: Complex -> Float
real (C x y) = x

im :: Complex -> Float
im (C x y) = y

magnitude :: Complex -> Float
magnitude = real . abs

dot :: Complex -> Float
dot (C x y) = x * x + y * y
