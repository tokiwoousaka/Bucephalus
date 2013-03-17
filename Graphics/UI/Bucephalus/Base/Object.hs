
module Graphics.UI.Bucephalus.Base.Object(
  ) where
import Graphics.UI.Bucephalus.Type.Shape.Collision
import Graphics.UI.Bucephalus.Type.Surface

data Object c = Object {
  objCollision :: c,
  position :: (Int, Int)
  }

instance Collision c => Collision (Object c) where
  l `collision` r = undefined --positionを意識した実装にする
