{-# LANGUAGE DeriveFunctor #-}

module Graphics.UI.Bucephalus.Type.Drawing where
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Shape.Movement
import Graphics.UI.Bucephalus.Type.Shape.Shape
import Control.Monad.Free

---------------------------------------------------------------------------------------------------
-- Definition of type
---------------------------------------------------------------------------------------------------

type Drawing p a = Free (DrawOrder p) a

data DrawOrder p a 
  = DrawPicture (Int, Int) p a 
  | DrawRect (Int, Int) (Int, Int) a  deriving (Show, Eq, Functor)

draw_picture :: (Int, Int) -> p -> Drawing p ()
draw_picture pos pic = liftF $ DrawPicture pos pic ()

draw_rect :: (Int, Int) -> (Int, Int) -> Drawing p ()
draw_rect p1 p2 = liftF $ DrawRect p1 p2 ()
