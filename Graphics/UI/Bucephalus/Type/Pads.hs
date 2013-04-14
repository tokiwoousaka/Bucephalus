
module Graphics.UI.Bucephalus.Type.Pads(
  GamePad(..),
  StandardPad(..),
  padToVector
  ) where

import Graphics.UI.Bucephalus.Type.Events

---------------------------------------------------------------------------------------------------
-- ゲームパッド型クラス Bucephalus内部で受け渡しされるゲームパッドやキーボードの状況
---------------------------------------------------------------------------------------------------

class GamePad p where
  padInit :: p
  interpretPadEvent :: p -> BucephalusEvent -> p

---------------------------------------------------------------------------------------------------
-- 基本的なゲームボタン型をデフォルトで提供
---------------------------------------------------------------------------------------------------

data StandardPad = StandardPad {
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

instance GamePad StandardPad where
  padInit = StandardPad 0 0 0 0 0 0 0 0 0 0 0 0

  interpretPadEvent = standerdPadEvent.incrementPushTime

--全てのボタンの押されている時間を加算
incrementPushTime :: StandardPad -> StandardPad
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
standerdPadEvent :: StandardPad -> BucephalusEvent -> StandardPad
standerdPadEvent pad (BucephalusKeyDown key) = keyDownEvent pad key
standerdPadEvent pad (BucephalusKeyUp   key) = keyUpEvent pad key
standerdPadEvent pad _                          = pad

---------------------------------------------------------------------------------------------------
-- キーが押された場合の処理

keyDownEvent :: StandardPad -> BucephalusKey -> StandardPad
keyDownEvent pad BucephalusKey_Down   = pad { buttonDown   = 1 }
keyDownEvent pad BucephalusKey_Up     = pad { buttonUp     = 1 }
keyDownEvent pad BucephalusKey_Left   = pad { buttonLeft   = 1 }
keyDownEvent pad BucephalusKey_Right  = pad { buttonRight  = 1 }
keyDownEvent pad BucephalusKey_z      = pad { buttonA      = 1 }
keyDownEvent pad BucephalusKey_x      = pad { buttonB      = 1 }
keyDownEvent pad BucephalusKey_c      = pad { buttonX      = 1 }
keyDownEvent pad BucephalusKey_v      = pad { buttonY      = 1 }
keyDownEvent pad BucephalusKey_a      = pad { buttonL      = 1 }
keyDownEvent pad BucephalusKey_s      = pad { buttonR      = 1 }
keyDownEvent pad BucephalusKey_Return = pad { buttonStart  = 1 }
keyDownEvent pad BucephalusKey_Esc    = pad { buttonSelect = 1 }
keyDownEvent pad _                    = pad

---------------------------------------------------------------------------------------------------
-- キーが放された場合の処理

keyUpEvent :: StandardPad -> BucephalusKey -> StandardPad
keyUpEvent pad BucephalusKey_Down   = pad { buttonDown   = 0 }
keyUpEvent pad BucephalusKey_Up     = pad { buttonUp     = 0 }
keyUpEvent pad BucephalusKey_Left   = pad { buttonLeft   = 0 }
keyUpEvent pad BucephalusKey_Right  = pad { buttonRight  = 0 }
keyUpEvent pad BucephalusKey_z      = pad { buttonA      = 0 }
keyUpEvent pad BucephalusKey_x      = pad { buttonB      = 0 }
keyUpEvent pad BucephalusKey_c      = pad { buttonX      = 0 }
keyUpEvent pad BucephalusKey_v      = pad { buttonY      = 0 }
keyUpEvent pad BucephalusKey_a      = pad { buttonL      = 0 }
keyUpEvent pad BucephalusKey_s      = pad { buttonR      = 0 }
keyUpEvent pad BucephalusKey_Return = pad { buttonStart  = 0 }
keyUpEvent pad BucephalusKey_Esc    = pad { buttonSelect = 0 }
keyUpEvent pad _                    = pad

---------------------------------------------------------------------------------------------------
-- StandardPad ユーティリティ
---------------------------------------------------------------------------------------------------

--押されてる矢印ボタンの状態からベクタ取得
padToVector :: StandardPad -> (Int, Int)
padToVector pad = (leftright pad, updown pad)
     
updown :: StandardPad -> Int
updown pad 
  | buttonUp pad > 0   = -1
  | buttonDown pad > 0 = 1
  | otherwise          = 0

leftright :: StandardPad -> Int
leftright pad 
  | buttonLeft  pad > 0 = -1
  | buttonRight pad > 0 = 1
  | otherwise           = 0
