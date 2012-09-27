
module Graphics.UI.Bucephalus.Type.Pads(
  GamePad(..),
  StanderdPad(..)
  ) where

import qualified Graphics.UI.SDL as SDL

---------------------------------------------------------------------------------------------------
-- ゲームパッド型クラス Bucephalus内部で受け渡しされるゲームパッドやキーボードの状況
---------------------------------------------------------------------------------------------------

class GamePad p where
  padInit :: p
  interpretPadEvent :: p -> SDL.Event -> p

---------------------------------------------------------------------------------------------------
-- 基本的なゲームボタン型をデフォルトで提供
---------------------------------------------------------------------------------------------------

data StanderdPad = StanderdPad {
  buttonUp :: Integer,
  buttonDown :: Integer,
  buttonLeft :: Integer,
  buttonRight :: Integer,
  buttonA :: Integer,
  buttonB :: Integer,
  buttonX :: Integer,
  buttonY :: Integer,
  buttonL :: Integer,
  buttonR :: Integer,
  buttonStart :: Integer,
  buttonSelect :: Integer
  } deriving (Show, Eq)

instance GamePad StanderdPad where
  padInit = StanderdPad 0 0 0 0 0 0 0 0 0 0 0 0

  interpretPadEvent pad _ = pad 
