
module Graphics.UI.Bucephalus.Base.Object(
  ObjectState(..),
  StandardObjectState(..)
  ) where
import Graphics.UI.Bucephalus.Type.Collision
import Graphics.UI.Bucephalus.Type.Surface

--Functor ((,) a) を下位モジュールに提供する
import Control.Monad.Instances

---------------------------------------------------------------------------------------------------
-- ObjectState型クラス ゲーム内で状態として受け渡されるキャラクターや障害物のようなオブジェクト
---------------------------------------------------------------------------------------------------

class (Collision o, CollisionType o) => ObjectState o where
  drawObject :: GameField -> o ->  IO ()

---------------------------------------------------------------------------------------------------
-- 最低限の機能を持つObjectを標準で提供
---------------------------------------------------------------------------------------------------

data StandardObjectState t = StandardObjectState {
  objectShape :: Shape,
  objectType :: t
  } deriving (Show, Read)

--当たり判定関連の型クラスインスタンス
instance Collision (StandardObjectState t) where
  l `collision` r = objectShape l `collision` objectShape r
instance CollisionType t => CollisionType (StandardObjectState t) where
  l `canOverlap` r = objectType l `canOverlap` objectType r

--オブジェクト型クラスのインスタンス
instance (Show t, CollisionType t) => ObjectState (StandardObjectState t) where
  drawObject _ o = putStrLn $ "Draw Object : " ++ show o
