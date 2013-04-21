
module Graphics.UI.Bucephalus.Type.Shape.Movement where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape.Shape 
import Graphics.UI.Bucephalus.Type.Shape.Collision (Point(..))

---------------------------------------------------------------------------------------------------
-- Definition of type
---------------------------------------------------------------------------------------------------

class Movement m where
  moveTo :: (Int, Int) -> m -> m
  movementPosition :: m -> (Int, Int)

  move :: (Int, Int) -> m -> m
  moveByPoint :: Point -> m -> m
  moveToPoint :: Point -> m -> m

  move (x, y) a = let
    (x', y') = movementPosition a
    in moveTo (x + x', y + y') a
  moveByPoint (Point p) = move p
  moveToPoint (Point p) = moveTo p

---------------------------------------------------------------------------------------------------
-- 各Shpeデータ型をMovementインスタンスにする
---------------------------------------------------------------------------------------------------

-- TODO
