module Graphics.UI.Bucephalus.Type.Collision(
  Point(..),
  Collision(..)
  ) where

---------------------------------------------------------------------------------------------------
-- 当たり判定型クラス、Bucephalus向けに当たり判定処理を提供する	
---------------------------------------------------------------------------------------------------

class Collision c where
  collision :: c -> c -> Bool

---------------------------------------------------------------------------------------------------
-- 点と点の当たり判定

--型定義
data Point a = Point (a, a) deriving (Show, Read)

instance Collision (Point a) where
  collision l r = undefined