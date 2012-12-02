module Graphics.UI.Bucephalus.Type.Collision(
  --型クラス
  Collision(..),
  CollisionType(..),
  --当たり判定枠提供
  Point(..),
  --当たり判定タイプ
  StandardCollisionType(..)
  ) where

---------------------------------------------------------------------------------------------------
-- 当たり判定型クラス、Bucephalus向けに当たり判定処理を提供する	
---------------------------------------------------------------------------------------------------

--当たり判定枠
class Collision c where
  collision :: c -> c -> Bool

--当たり判定タイプ
class CollisionType t where
  canOverlap :: t -> t -> Bool

---------------------------------------------------------------------------------------------------
-- 点と点の当たり判定

--型定義
data Point = Point (Int, Int) deriving (Show, Read)

instance Collision Point where
  collision l r = undefined

---------------------------------------------------------------------------------------------------
-- 基本的な当たり判定タイプを標準で提供
---------------------------------------------------------------------------------------------------
--通常のマップ用当たり判定枠

--壁、地面、キャラクター、カーソルの三種類を提供
data StandardCollisionType = TypeWall | TypeGround | TypeChar | TypeCursor

--キャラクターと壁／キャラクターとキャラクターは重なれない
instance CollisionType StandardCollisionType where
  TypeWall `canOverlap` TypeChar = False
  TypeChar `canOverlap` TypeChar = False
  _        `canOverlap` _        = True

--unitをインスタンスにする事で、メニュー等の当たり判定タイプが不要な場合に対応
instance CollisionType () where
  _ `canOverlap` _ = True
