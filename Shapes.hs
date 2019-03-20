-- use `(..)` to export all value constructor of the data type
-- Shape(..) is the same as Shape(Circle, Rectangle)
--
module Shapes
( Point(..)
{- `, Shape` to only export the data type without its value constructors -}
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where

data Point = Point Float Float deriving Show
data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving Show

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float {-moveX-} -> Float {-moveY-} -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (y + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy =
  Rectangle
    (Point (x1 + dx) (y1 + dy))
    (Point (x2 + dx) (y2 + dy))

origin :: Point
origin = Point 0 0

baseCircle :: Float {-radius-} -> Shape
baseCircle r = Circle origin r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle origin (Point width height)
