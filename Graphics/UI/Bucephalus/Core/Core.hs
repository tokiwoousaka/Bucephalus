{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Graphics.UI.Bucephalus.Core.Core(
  --型の提供
  GameState(..),
  SubstancesCore(..),
  --主処理
  coreStart
  ) where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Type.Pads

--SDL関連
import qualified Graphics.UI.SDL as SDL

--Haskell標準
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

-- 状態管理／ボタン押下状況も同時に受け渡す
data GameState p s = GameState {
  buttons    :: p,
  takeState  :: s
  }

-- 処理するプログラム本体受け渡し
data SubstancesCore p s = SubstancesCore {
  initSC :: IO s,
  mainSC :: GameState p s -> IO (GameState p s),
  quitSC :: s -> IO ()
  }

---------------------------------------------------------------------------------------------------
-- コア
---------------------------------------------------------------------------------------------------

coreStart :: GamePad p => SubstancesCore p s -> IO ()
coreStart substances = do
  --初期化処理
  initSDL
  state <- initSC substances >>= return . GameState padInit
  --メインループ
  (_, final) <- SDL.getTicks >>= mainLoop (substances, state)
  --終了
  quitSC substances $ takeState final
  quitSDL

mainLoop :: GamePad p => 
  (SubstancesCore p s, GameState p s) -> Word32 -> IO (SubstancesCore p s, GameState p s) 
mainLoop (substances, state@(GameState pad st)) ago = do
  SDL.delay 1 --CPU負担軽減
  --フレーム間隔を一定に保つ
  t <- SDL.getTicks
  if ago + 16 > t then mainLoop (substances, state) ago
   else do
    --ゲーム処理実行
    ev <- SDL.pollEvent
    nPad <- return $ interpretPadEvent pad ev
    nState <- mainSC substances $ GameState nPad st
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
quitSDL = SDL.quit
