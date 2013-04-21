
module Graphics.UI.Bucephalus.Base.Object where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Shape
import Graphics.UI.Bucephalus.Type.Drawing
import qualified Graphics.UI.Bucephalus.Type.InfList as INF

---------------------------------------------------------------------------------------------------
-- Definition of type
---------------------------------------------------------------------------------------------------
-- キャラクターや障害物、画面内のイメージ画像等を保持するオブジェクト

data GameObject d c = GameObject {
  objPosition :: (Int, Int),
  objStatus :: ObjectStatus d c,
  objFreames :: INF.List (ObjectStatus d c)
  }

--衝突判定
instance (Movement c, Collision c) => Collision (GameObject d c) where
  l `collision` r = moveCollision l `collision` moveCollision r
    where moveCollision o = move (objPosition o) $ (objCollision . objStatus) o

--オブジェクト移動
instance Movement (GameObject d c) where
  moveTo (x, y) o = o { objPosition = (x, y) }
  movementPosition = objPosition

--オブジェクトのフレーム遷移（アニメーション）
objNextFrame :: GameObject d c -> GameObject d c
objNextFrame o = o { 
  objStatus  = INF.head (objFreames o),
  objFreames = INF.tail (objFreames o) }

---------------------------------------------------------------------------------------------------
--オブジェクトの瞬間的な状態

data ObjectStatus d c = ObjectStatus {
  objCollision :: c,
  objDrawInfo :: Drawing d ()
  }

