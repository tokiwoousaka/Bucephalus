
module Graphics.UI.Bucephalus.Base.Object(
  GameObject(..)
  ) where
import Graphics.UI.Bucephalus.Type.Collision
import Graphics.UI.Bucephalus.Type.Surface

class GameObject o where
  drawObject :: GameField -> o ->  IO ()
