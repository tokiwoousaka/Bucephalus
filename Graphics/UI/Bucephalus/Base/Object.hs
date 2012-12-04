
module Graphics.UI.Bucephalus.Base.Object(
  ObjectState(..),
  GameObject(..),
  StandardObjectState(..),
  StandardGameObject(..)
  ) where
import Graphics.UI.Bucephalus.Type.Collision
import Graphics.UI.Bucephalus.Type.Surface

---------------------------------------------------------------------------------------------------
-- Object型クラス ゲーム内で状態として受け渡されるキャラクターや障害物のようなオブジェクト
---------------------------------------------------------------------------------------------------

class ObjectState o where
  drawObject :: GameField -> o ->  IO ()

class GameObject m where
  fixObject    :: ObjectState o => m o -> o
  revertObject :: ObjectState o => m o -> o 
  moveObject   :: ObjectState o => (o -> o) -> m o -> m o

---------------------------------------------------------------------------------------------------
-- 最低限の機能を持つObjectを標準で提供
---------------------------------------------------------------------------------------------------

data StandardObjectState = StandardObjectState deriving (Show, Read)
instance ObjectState StandardObjectState where
  drawObject _ _ = undefined

data StandardGameObject a = StandardGameObject (a, a) deriving (Show, Read)
instance GameObject StandardGameObject where
  fixObject    (StandardGameObject (x, _)) = x
  revertObject (StandardGameObject (_, x)) = x
  moveObject f (StandardGameObject (x, y)) = StandardGameObject (x, f y)
