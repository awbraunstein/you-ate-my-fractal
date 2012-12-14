module QTree where

import Control.Monad

data Quad a = Q{ ul :: a,
                 ur :: a,
                 lr :: a,
                 ll :: a }
              deriving (Show)

type Neighbor a = QTree a

type Range = Quad Float

data Child = UL | UR | LR | LL | ROOT
    deriving (Show)

data QTree a = Branch a (Quad (QTree a)) (Quad (Neighbor a)) Range
             | Empty
               deriving (Show)


range :: QTree a -> Maybe Range
range (Branch _ _ _ r) = Just r
range Empty = Nothing

bound :: (Quad Float -> Float) -> QTree a -> Maybe Float
bound fn = liftM fn . range

x1Bound :: QTree a -> Maybe Float
x1Bound = bound ul

x2Bound :: QTree a -> Maybe Float
x2Bound = bound lr

y1Bound :: QTree a -> Maybe Float
y1Bound = bound ur

y2Bound :: QTree a -> Maybe Float
y2Bound = bound ll

children :: QTree a -> Maybe (Quad (QTree a))
children (Branch _ q _ _) = Just q
children Empty = Nothing

child :: (Quad (QTree a) -> QTree a) -> QTree a -> Maybe (QTree a)
child fn = liftM fn . children

ulC :: QTree a -> Maybe (QTree a)
ulC = child ul

urC :: QTree a -> Maybe (QTree a)
urC = child ur

lrC :: QTree a -> Maybe (QTree a)
lrC = child lr

llC :: QTree a -> Maybe (QTree a)
llC = child ll

nodeVal :: QTree a -> Maybe a
nodeVal Empty = Nothing
nodeVal (Branch v _ _ _) = Just v

midpoint :: Range -> (Float, Float)
midpoint (Q x1 y1 x2 y2) = ((x2 + x1)/2, (y2 + y1)/2)

ulQ :: Range -> Range
ulQ r@(Q x1 y1 _ _) = (Q x1 y1 mx my) where
  (mx, my) = midpoint r

urQ :: Range -> Range
urQ r@(Q _ y1 x2 _) = (Q mx y1 x2 my) where
  (mx, my) = midpoint r

lrQ :: Range -> Range
lrQ r@(Q _ _ x2 y2) = (Q mx my x2 y2) where
  (mx, my) = midpoint r

llQ :: Range -> Range
llQ r@(Q x1 _ _ y2) = (Q x1 my mx y2) where
  (mx, my) = midpoint r

neighbors :: QTree a -> Maybe (Quad (Neighbor a))
neighbors (Branch _ _ n _) = Just n
neighbors Empty = Nothing

emptyQ = (Q Empty Empty Empty Empty)

neighbor :: (Quad (Neighbor a) -> Neighbor a) -> QTree a -> Maybe (Neighbor a)
neighbor fn = liftM fn . neighbors

topN :: QTree a -> Maybe (Neighbor a)
topN = neighbor top

leftN :: QTree a -> Maybe (Neighbor a)
leftN = neighbor left

rightN :: QTree a -> Maybe (Neighbor a)
rightN = neighbor right

bottomN :: QTree a -> Maybe (Neighbor a)
bottomN = neighbor bottom

pointInRange :: (Float, Float) -> Range -> Bool
pointInRange (x, y) (Q x1 y1 x2 y2) = x <= x2 && x >= x1 && y >= y2 && y <= y1


intersect :: Range -> Range -> Bool
intersect (Q x1a y1a x2a y2a) (Q x1b y1b x2b y2b) =
  (x1a <= x2b && x2a >= x1b && y1a >= y2b && y2a <= y1b)


top :: Quad a -> a
top = ul

left :: Quad a -> a
left = ur

right :: Quad a -> a
right = lr

bottom :: Quad a -> a
bottom = ll
