
module Graphics.UI.Bucephalus.Base.Object(
  ) where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape
import Graphics.UI.Bucephalus.Type.Surface

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data Object c = Object {
  objCollision :: c,
  position :: (Int, Int)
  }

--衝突判定
instance (Collision c, Movement c) => Collision (Object c) where
  l `collision` r = moveCollision l `collision` moveCollision r
    where moveCollision o = move (position o) $ objCollision o

--オブジェクト移動
instance Movement (Object c) where
  move (x, y) o = let
    (ox, oy) = position o
    in o { position = (x + ox, y + oy) }
