module Graphics.UI.Bucephalus.Type.Collision(
  --型クラス
  Collision(..),
  CollisionType(..),
  --CollisionSet(..), --廃止？
  --当たり判定枠提供
  Point(..),
  Rectangle(..),
  Shape(..),
  --当たり判定タイプ
  StandardCollisionType(..),
  --当たり判定コレクション
  --StandardCollisionSet(..) --廃止？
  ) where

import Data.List
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

----TODO 考え中
--当たり判定セット、CollisionとCollisionTypeを包括
--class Collision c => CollisionSet c where
--  collisionTypeFrom :: CollisionType a => c a b -> a
--  collisionFrom :: Collision b => c a b -> b

---------------------------------------------------------------------------------------------------
-- 個々の当たり判定インスタンスの作成
---------------------------------------------------------------------------------------------------
-- 点と点の当たり判定

--型定義
data Point = Point (Int, Int) deriving (Show, Read, Eq)

--インスタンス作成 
instance Collision Point where collision l r = l == r 
 
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
-- 円と円の当たり判定
 
--型定義
data Circle = Circle (Int,Int) Int deriving (Show, Read, Eq)

--インスタンス作成
instance Collision Circle where
  collision cir1 cir2 = let
    (Circle (x0,y0) radius0) = cir1
    (Circle (x1,y1) radius1) = cir2
    in (x0 - x1)*(x0 - x1) + (y0 - y1)*(y0 - y1) <= (radius0 + radius1)*(radius0 + radius1)

---------------------------------------------------------------------------------------------------
-- 当たり判定枠統合
---------------------------------------------------------------------------------------------------

-- Shape型 各々の形状を統合した型
data Shape = ShapePoint Point | ShapeRectangle Rectangle deriving (Show, Read, Eq)

instance Collision Shape where
  --同型同士の当たり判定
  (ShapePoint l) `collision` (ShapePoint r) = l `collision` r
  (ShapeRectangle l) `collision` (ShapeRectangle r) = l `collision` r

---------------------------------------------------------------------------------------------------
-- 基本的な型を標準で提供
---------------------------------------------------------------------------------------------------
--通常のマップ用当たり判定枠

--壁、地面、キャラクター、カーソルの三種類を提供
data StandardCollisionType = 
  TypeWall | TypeGround | TypeChar | TypeCursor deriving (Show, Read, Eq)

--キャラクターと壁／キャラクターとキャラクターは重なれない
instance CollisionType StandardCollisionType where
  TypeWall `canOverlap` TypeChar = False
  TypeChar `canOverlap` TypeChar = False
  TypeChar `canOverlap` x        = (flip canOverlap) x TypeChar
  _        `canOverlap` _        = True

--unitをインスタンスにする事で、メニュー等の当たり判定タイプが不要な場合に対応
instance CollisionType () where
  _ `canOverlap` _ = True

---------------------------------------------------------------------------------------------------

--Cliision型クラスインスタンスのリストもCollision型クラスインスタンス
instance (Eq a, Collision a) => Collision [a] where
  ls `collision` rs = all id . map (uncurry collision) $ nub [(l, r) | l <- ls, r <- rs]

----二値のタプルはCollisionSetのインスタンス
instance Collision c => Collision ((,) t c) where
  ls `collision` rs = snd ls `collision` snd rs
----TODO 考え中
--instance CollisionSet (,) where
--  collisionTypeFrom xs = undefined
--  collisionFrom x = snd x

