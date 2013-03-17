
module Graphics.UI.Bucephalus.Type.Shape.Shape where

---------------------------------------------------------------------------------------------------

--点
data Point = Point (Int, Int) deriving (Show, Read, Eq)

--線
data Line = Line (Point, Point) deriving (Show, Read, Eq)

--四角形
data Rectangle = Rectangle Point Point deriving (Show, Read, Eq)

--円
data Circle = Circle Point Int deriving (Show, Read, Eq)

---------------------------------------------------------------------------------------------------
-- Shape型 各々の形状を統合した型
data Shape 
  = ShapePoint Point 
  | ShapeLine Line
  | ShapeRectangle Rectangle
  | ShapeCircle Circle deriving (Show, Read, Eq)

---------------------------------------------------------------------------------------------------
--各形状からShape型への変換

pointToShape :: Point -> Shape
pointToShape p = ShapePoint p

lineToShape :: Line -> Shape
lineToShape l = ShapeLine l

rectangleToShape :: Rectangle -> Shape
rectangleToShape r = ShapeRectangle r

circleToShape :: Circle -> Shape
circleToShape c = ShapeCircle c

---------------------------------------------------------------------------------------------------
--Shae型から各形状への変換

shapeToPoint :: Shape -> Maybe Point
shapeToPoint (ShapePoint p) = Just p
shapeToPoint _              = Nothing

shapeToLine :: Shape -> Maybe Line
shapeToLine (ShapeLine l) = Just l
shapeToLine _             = Nothing

shapeToRectangle :: Shape -> Maybe Rectangle
shapeToRectangle (ShapeRectangle r) = Just r
shapeToRectangle _                  = Nothing

shapeToCircle :: Shape -> Maybe Circle
shapeToCircle (ShapeCircle c) = Just c
shapeToCircle _               = Nothing
