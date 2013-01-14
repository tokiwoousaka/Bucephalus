module Graphics.UI.Bucephalus.Type.Collision(
  --型クラス
  Collision(..),
  CollisionType(..),
  CollisionSet(..),
  --当たり判定枠提供
  Point(..),
  Rectangle(..),
  Shape(..),
  --当たり判定タイプ
  StandardCollisionType(..),
  --当たり判定コレクション
  StandardCollisionSet(..)
  ) where

--(,) a をFunctorとして利用する
import Control.Monad.Instances

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

--当たり判定枠
class Collision c where
  collision :: c -> c -> Bool

--当たり判定タイプ
class CollisionType t where
  canOverlap :: t -> t -> Bool 

--当たり判定コレクション、複数の(Collision, CollisionType)のペアを一括で扱う
class CollisionSet c where
  --CollisionType別全ての衝突判定
  collisionA :: CollisionType t => c -> c -> [(t, t)] 
  --canOverlap = Falseの組み合わせのみ判定
  collisionC :: CollisionType t => c -> c -> [(t, t)]

---------------------------------------------------------------------------------------------------
-- 個々の当たり判定インスタンスの作成
---------------------------------------------------------------------------------------------------
-- 点と点の当たり判定

--型定義
data Point = Point (Int, Int) deriving (Show, Read, Eq)

--インスタンス作成 
instance Collision Point where
  collision l r = l == r 
 
---------------------------------------------------------------------------------------------------
-- 四角形と四角形の当たり判定
 
--型定義
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Show, Read, Eq)

--インスタンス作成 
instance Collision Rectangle where
  collision rect1 rect2 = let
    (Rectangle (x0, y0) (x1, y1)) = rect1
    (Rectangle (x2, y2) (x3, y3)) = rect2
    in (x0 < x3 && x2 < x1)&&(y0 < y3 && y2 < y1)

---------------------------------------------------------------------------------------------------
-- Shape型 各々の形状を統合した型
---------------------------------------------------------------------------------------------------

data Shape = ShapeRectangle Rectangle deriving (Show, Read, Eq)

instance Collision Shape where
  --同型同士の当たり判定
  (ShapeRectangle l) `collision` (ShapeRectangle r) = l `collision` r

---------------------------------------------------------------------------------------------------
-- 基本的な当たり判定タイプを標準で提供
---------------------------------------------------------------------------------------------------
--通常のマップ用当たり判定枠

--壁、地面、キャラクター、カーソルの三種類を提供
data StandardCollisionType = 
  TypeWall | TypeGround | TypeChar | TypeCursor deriving (Show, Read, Eq)

--キャラクターと壁／キャラクターとキャラクターは重なれない
instance CollisionType StandardCollisionType where
  TypeWall `canOverlap` TypeChar = False
  TypeChar `canOverlap` TypeChar = False
  _        `canOverlap` _        = True

--unitをインスタンスにする事で、メニュー等の当たり判定タイプが不要な場合に対応
instance CollisionType () where
  _ `canOverlap` _ = True

---------------------------------------------------------------------------------------------------
-- 基本的なCollisionCollectionの型を標準で提供
---------------------------------------------------------------------------------------------------

--コレクションをリストで扱う
data StandardCollisionSet t c = StandardCollisionSet [(t, c)]

instance Functor (StandardCollisionSet t) where
  fmap f (StandardCollisionSet x) = StandardCollisionSet $ fmap (fmap f) x
instance CollisionSet (StandardCollisionSet t c) where
  collisionC = undefined
  collisionA = undefined
