
module Graphics.UI.Bucephalus.Type.Pads(
  GamePad(..),
  StanderdPad(..),
  padToVector
  ) where

import Graphics.UI.SDL

---------------------------------------------------------------------------------------------------
-- ゲームパッド型クラス Bucephalus内部で受け渡しされるゲームパッドやキーボードの状況
---------------------------------------------------------------------------------------------------

class GamePad p where
  padInit :: p
  interpretPadEvent :: p -> Event -> p

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

  interpretPadEvent = standerdPadEvent.incrementPushTime

--全てのボタンの押されている時間を加算
incrementPushTime :: StanderdPad -> StanderdPad
incrementPushTime pad = pad {
  buttonUp     = incTime $ buttonUp pad,
  buttonDown   = incTime $ buttonDown pad,
  buttonLeft   = incTime $ buttonLeft pad,
  buttonRight  = incTime $ buttonRight pad,
  buttonA      = incTime $ buttonA pad,
  buttonB      = incTime $ buttonB pad,
  buttonX      = incTime $ buttonX pad,
  buttonY      = incTime $ buttonY pad,
  buttonL      = incTime $ buttonL pad,
  buttonR      = incTime $ buttonR pad,
  buttonStart  = incTime $ buttonStart pad,
  buttonSelect = incTime $ buttonSelect pad}
    where
      incTime :: Integer -> Integer
      incTime 0 = 0
      incTime x = x + 1

---------------------------------------------------------------------------------------------------
--押されたキーに応じてパッドの状態を変更して返す
standerdPadEvent :: StanderdPad -> Event -> StanderdPad
standerdPadEvent pad (KeyDown (Keysym key _ _)) = keyDownEvent pad key
standerdPadEvent pad (KeyUp (Keysym key _ _))   = keyUpEvent pad key
standerdPadEvent pad _                          = pad

---------------------------------------------------------------------------------------------------
-- キーが押された場合の処理

keyDownEvent :: StanderdPad -> SDLKey -> StanderdPad
keyDownEvent pad SDLK_DOWN  = pad { buttonDown  = 1 }
keyDownEvent pad SDLK_UP    = pad { buttonUp    = 1 }
keyDownEvent pad SDLK_LEFT  = pad { buttonLeft  = 1 }
keyDownEvent pad SDLK_RIGHT = pad { buttonRight = 1 }

---------------------------------------------------------------------------------------------------
-- キーが放された場合の処理

keyUpEvent :: StanderdPad -> SDLKey -> StanderdPad
keyUpEvent pad SDLK_DOWN  = pad { buttonDown  = 0 }
keyUpEvent pad SDLK_UP    = pad { buttonUp    = 0 }
keyUpEvent pad SDLK_LEFT  = pad { buttonLeft  = 0 }
keyUpEvent pad SDLK_RIGHT = pad { buttonRight = 0 }

---------------------------------------------------------------------------------------------------
-- StanderdPad ユーティリティ
---------------------------------------------------------------------------------------------------

--押されてる矢印ボタンの状態からベクタ取得
padToVector :: StanderdPad -> (Int, Int)
padToVector pad = (leftright pad, updown pad)
     
updown :: StanderdPad -> Int
updown pad 
  | buttonUp pad > 0   = -1
  | buttonDown pad > 0 = 1
  | otherwise          = 0

leftright :: StanderdPad -> Int
leftright pad 
  | buttonLeft  pad > 0 = -1
  | buttonRight pad > 0 = 1
  | otherwise           = 0
