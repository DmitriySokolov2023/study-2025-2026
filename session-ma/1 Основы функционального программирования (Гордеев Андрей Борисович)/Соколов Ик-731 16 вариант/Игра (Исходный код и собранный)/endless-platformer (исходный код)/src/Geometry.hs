module Geometry
  ( Rect (..)
  , Interval (..)
  , rectLeft
  , rectRight
  , rectTop
  , rectBottom
  , rectIntersects
  , intervalContains
  , intervalOverlaps
  ) where

data Rect = Rect
  { rectX :: Float
  , rectY :: Float
  , rectW :: Float
  , rectH :: Float
  }
  deriving (Eq, Show)

data Interval = Interval
  { intA :: Float
  , intB :: Float
  }
  deriving (Eq, Show)

rectLeft :: Rect -> Float
rectLeft r = rectX r - rectW r / 2

rectRight :: Rect -> Float
rectRight r = rectX r + rectW r / 2

rectTop :: Rect -> Float
rectTop r = rectY r + rectH r / 2

rectBottom :: Rect -> Float
rectBottom r = rectY r - rectH r / 2

rectIntersects :: Rect -> Rect -> Bool
rectIntersects a b =
  not (rectRight a < rectLeft b
        || rectLeft a > rectRight b
        || rectTop a < rectBottom b
        || rectBottom a > rectTop b)

intervalContains :: Float -> Interval -> Bool
intervalContains x i = x >= intA i && x <= intB i

intervalOverlaps :: Float -> Float -> Interval -> Bool
intervalOverlaps x1 x2 i =
  intB i > x1 && intA i < x2