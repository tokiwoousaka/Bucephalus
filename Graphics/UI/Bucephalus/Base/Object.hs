
module Graphics.UI.Bucephalus.Base.Object where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape
import qualified Graphics.UI.Bucephalus.Type.InfList as INF

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------
--描画情報型クラス ...Typeモジュールに移動したほうが良いかも

class DrawInfomation d where
  --drawInfo :: TODO 考え中

---------------------------------------------------------------------------------------------------
-- キャラクターや障害物、画面内のイメージ画像等を保持するオブジェクト

data GameObject d c = GameObject {
  position :: (Int, Int),
  objStatus :: ObjectStatus d c,
  objFreames :: INF.List (ObjectStatus d c)
  }

--衝突判定
instance (Collision c, Movement c) => Collision (GameObject d c) where
  l `collision` r = moveCollision l `collision` moveCollision r
    where moveCollision o = move (position o) $ (objCollision . objStatus) o

--オブジェクト移動
instance Movement (GameObject d c) where
  move (x, y) o = let
    (ox, oy) = position o
    in o { position = (x + ox, y + oy) }

---------------------------------------------------------------------------------------------------
--オブジェクトの瞬間的な状態

data ObjectStatus d c = ObjectStatus {
  objCollision :: c,
  objDrawInfo :: d
  }

