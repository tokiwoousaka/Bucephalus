
module Graphics.UI.Bucephalus.Base.Object(
  ObjectState(..),
  StandardObjectState(..),
  --StandardGameObject(..)
  ) where
import Graphics.UI.Bucephalus.Type.Collision
import Graphics.UI.Bucephalus.Type.Surface

--Functor ((,) a) を下位モジュールに提供する
import Control.Monad.Instances

---------------------------------------------------------------------------------------------------
-- Object型クラス ゲーム内で状態として受け渡されるキャラクターや障害物のようなオブジェクト
---------------------------------------------------------------------------------------------------

class ObjectState o where
  drawObject :: GameField -> o ->  IO ()

---------------------------------------------------------------------------------------------------
-- 最低限の機能を持つObjectを標準で提供
---------------------------------------------------------------------------------------------------

data StandardObjectState = StandardObjectState String deriving (Show, Read)
instance ObjectState StandardObjectState where
  drawObject _ (StandardObjectState o) = putStrLn $ "Draw Object : " ++ o
