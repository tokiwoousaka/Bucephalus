
module Graphics.UI.Bucephalus.Type.Shape.Movement where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape.Shape 
import Graphics.UI.Bucephalus.Type.Shape.Collision (Point(..))

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

class Movement m where
  move :: (Int, Int) -> m -> m
  moveByPoint :: Point -> m -> m

  moveByPoint (Point p) = move p

---------------------------------------------------------------------------------------------------
-- 各Shpeデータ型をMovementインスタンスにする
---------------------------------------------------------------------------------------------------
