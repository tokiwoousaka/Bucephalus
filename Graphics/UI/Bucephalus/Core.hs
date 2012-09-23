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
data GameState b s = GameState {
  buttons    :: b,
  takeState  :: s
  }

-- 処理するプログラム本体受け渡し
data SubstancesCore b s = SubstancesCore {
  initSC :: IO s,
  mainSC :: GameState b s -> IO (GameState b s),
  quitSC :: s -> IO ()
  }

---------------------------------------------------------------------------------------------------
-- コア
---------------------------------------------------------------------------------------------------

coreStart :: SubstancesCore ButtonsState s -> IO () --TODO ButtonsStateを型変数にする
coreStart substances = do
  --初期化処理
  initSDL
  state <- initSC substances >>= return . GameState initButtonState --TODO 初期化関数外だし
  --メインループ
  (_, final) <- SDL.getTicks >>= mainLoop (substances, state)
  --終了
  quitSC substances $ takeState final
  quitSDL

mainLoop :: (SubstancesCore b s, GameState b s) -> Word32 -> IO (SubstancesCore b s, GameState b s) 
mainLoop (substances, state) ago = do
  SDL.delay 1 --CPU負担軽減
  --フレーム間隔を一定に保つ
  t <- SDL.getTicks
  if ago + 16 > t then mainLoop (substances, state) ago
   else do
    --ゲーム処理実行
    ev <- SDL.pollEvent
    nState <- mainSC substances $ state
    --終了条件判定、再帰
    if isQuit ev then return (substances, nState) else mainLoop (substances, nState) t

  where
    --終了条件判定
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyUp key) = SDL.symKey key == SDL.SDLK_ESCAPE --ESCキー押下
    isQuit ev              = ev             == SDL.Quit        --画面が閉じられる
    isQuit _               = False

---------------------------------------------------------------------------------------------------
--SDL初期化
initSDL :: IO ()
initSDL = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 [] 
  return ()

--終了
quitSDL :: IO ()
quitSDL = do
  SDL.setVideoMode 640 480 32 [] 
  SDL.quit

---------------------------------------------------------------------------------------------------
-- ゲームボタン管理
---------------------------------------------------------------------------------------------------
initButtonState :: ButtonsState
initButtonState = ButtonsState 0 0 0 0 0 0 0 0 0 0

