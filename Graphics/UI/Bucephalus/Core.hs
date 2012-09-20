---------------------------------------------------------------------------------------------------
-- 
-- Core Program in The Project Bucephalus (C)2012 Tokiwo Ousaka
-- 
---------------------------------------------------------------------------------------------------

module Graphics.UI.Bucephalus.Core(
  --型の提供
  ButtonsState(..),
  GameState(..),
  SubstancesCore(..),
  --主処理
  coreStart
  ) where

--SDL関連
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Image      as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr

--Haskell標準
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

-- ゲームボタン管理
-- ゲームパッドへの対応を考慮し、キーの名前で管理しない
data GameButton = Up | Down | Left | Right | A | B | X | Y | L | R

--ボタンが押されてからの長さをフレーム数で管理
data ButtonsState = ButtonsState {
  upButton    :: Integer,
  downButton  :: Integer,
  leftButton  :: Integer,
  rightButton :: Integer,
  aButton     :: Integer,
  bButton     :: Integer,
  xButton     :: Integer,
  yButton     :: Integer,
  lButton     :: Integer,
  rButton     :: Integer
  } deriving (Show, Eq)

-- 状態管理／ボタン押下状況も同時に受け渡す
data GameState a = GameState {
  buttons    :: ButtonsState,
  gameState  :: a
  }

-- 処理するプログラム本体受け渡し
data SubstancesCore a = SubstancesCore {
  initSC :: IO a,
  mainSC :: GameState a-> IO (GameState a),
  quitSC :: a -> IO ()
  }

---------------------------------------------------------------------------------------------------
-- コア
---------------------------------------------------------------------------------------------------
coreStart :: SubstancesCore a -> IO ()
coreStart substances = putStrLn "Called coreStart function!" --TODO 処理記述

---------------------------------------------------------------------------------------------------
-- ゲームボタン管理
---------------------------------------------------------------------------------------------------
makeButtonState :: Maybe GameButton -> ButtonsState
makeButtonState _ = ButtonsState 0 0 0 0 0 0 0 0 0 0

