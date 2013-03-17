{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Graphics.UI.Bucephalus.Type.Shape.Collision(
  --型クラス
  Collision(..),
  CollisionType(..),
  CollisionSet(..),
  --当たり判定枠提供
  Point(..),
  Rectangle(..),
  Shape(..),
  --当たり判定属性
  StandardCollisionType(..),
  ) where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape.Shape

--Haskell標準
import Data.List (nub)

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

--当たり判定枠
class Collision c where
  collision :: c -> c -> Bool

--当たり判定タイプ
class CollisionType t where
  canOverlap :: t -> t -> Bool 

--当たり判定セット、CollisionとCollisionTypeを包括
class (Collision c) => CollisionSet c a b | c -> a , c -> b where
  collisionTypeFrom :: c -> a
  collisionFrom :: c -> b

---------------------------------------------------------------------------------------------------
-- 各Shapeデータ型をCollisionインスタンスにする
---------------------------------------------------------------------------------------------------
-- 点と点の当たり判定

instance Collision Point where collision l r = l == r 
 
---------------------------------------------------------------------------------------------------
-- 線と線の当たり判定
 
instance Collision Line where
  collision line1 line2 = let
    --パターンマッチで座標取り出し
    (Line (point0, point1)) = line1
    (Line (point2, point3)) = line2
    (Point (x0, y0)) = point0
    (Point (x1, y1)) = point1
    (Point (x2, y2)) = point2
    (Point (x3, y3)) = point3
    --当たり判定計算式
    --TODO tokiwoousaka アルゴリズム確認
    startEndPoint px py = (x0 - x1) * (py - y0) + (y0 - y1) * (x0 - px)
    in startEndPoint x2 y2 * startEndPoint x3 y3 > 0
    --旧処理、SIwatsukiと内容確認した後削除
    --line1StartEndPoint = (x0 - x1) * (y2 - y0) + (y0 - y1) * (x0 - x2)
    --line2StartEndPoint = (x0 - x1) * (y3 - y0) + (y0 - y1) * (x0 - x3) 
    --in line1StartEndPoint * line2StartEndPoint > 0

---------------------------------------------------------------------------------------------------
-- 四角形と四角形の当たり判定
 
instance Collision Rectangle where
  collision rect1 rect2 = let
    (Rectangle point0 point1) = rect1
    (Rectangle point2 point3) = rect2
    (Point (x0, y0)) = point0
    (Point (x1, y1)) = point1
    (Point (x2, y2)) = point2
    (Point (x3, y3)) = point3
    in (x0 < x3 && x2 < x1)&&(y0 < y3 && y2 < y1)

---------------------------------------------------------------------------------------------------
-- 円と円の当たり判定
 
instance Collision Circle where
  collision cir1 cir2 = let
    (Circle point0 radius0) = cir1
    (Circle point1 radius1) = cir2
    (Point (x0, y0)) = point0
    (Point (x1, y1)) = point1
    in (x0 - x1)*(x0 - x1) + (y0 - y1)*(y0 - y1) <= (radius0 + radius1)*(radius0 + radius1)

---------------------------------------------------------------------------------------------------
-- 当たり判定枠統合
---------------------------------------------------------------------------------------------------

instance Collision Shape where
  --同型同士の当たり判定
  (ShapePoint l) `collision` (ShapePoint r) = l `collision` r
  (ShapeLine l) `collision` (ShapeLine r) = l `collision` r
  (ShapeRectangle l) `collision` (ShapeRectangle r) = l `collision` r
  (ShapeCircle l) `collision` (ShapeCircle r) = l `collision` r

---------------------------------------------------------------------------------------------------
-- 基本的な型を標準で提供
---------------------------------------------------------------------------------------------------
--通常のマップ用当たり判定枠

--壁、地面、キャラクター、カーソルの三種類を提供
data StandardCollisionType = 
  TypeWall | TypeGround | TypeChar | TypeImage deriving (Show, Read, Eq)

--キャラクターと壁／キャラクターとキャラクターは重なれない
instance CollisionType StandardCollisionType where
  TypeWall `canOverlap` TypeChar = False
  TypeChar `canOverlap` TypeChar = False
  TypeChar `canOverlap` x        = x `canOverlap` TypeChar
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
instance (CollisionType t, Collision c) => CollisionSet ((,) t c) t c  where
  collisionTypeFrom x = fst x 
  collisionFrom x = snd x

