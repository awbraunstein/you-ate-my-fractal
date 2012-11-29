module QTree where

import Control.Monad

data Quad a = Q a a a a

type Neighbor a = QTree a

type Range = Quad Float

data QTree a = Branch a (Quad (QTree a)) (Quad (Neighbor a)) Range
             | Empty

range :: QTree a -> Maybe Range
range (Branch _ _ _ r) = Just r
range Empty = Nothing

bound :: (Quad Float -> Float) -> QTree a -> Maybe Float
bound fn = liftM fn . range

ulBound :: QTree a -> Maybe Float
ulBound = bound ul

urBound :: QTree a -> Maybe Float
urBound = bound ur

lrBound :: QTree a -> Maybe Float
lrBound = bound lr

llBound :: QTree a -> Maybe Float
llBound = bound ll

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

neighbors :: QTree a -> Maybe (Quad (Neighbor a))
neighbors (Branch _ _ n _) = Just n
neighbors Empty = Nothing

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



-- Utilities for acting on Quads. Avoid calling these directly
ul :: Quad a -> a
ul (Q x _ _ _) = x

ur :: Quad a -> a
ur (Q _ x _ _) = x

lr :: Quad a -> a
lr (Q _ _ x _) = x

ll :: Quad a -> a
ll (Q _ _ _ x) = x

top :: Quad a -> a
top = ul

left :: Quad a -> a
left = ur

right :: Quad a -> a
right = lr

bottom :: Quad a -> a
bottom = ll
