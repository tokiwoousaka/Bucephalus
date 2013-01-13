{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Graphics.UI.Bucephalus.Core.Core(
  --型の提供
  GameState(..),
  --主処理
  coreStart,
  --Config
  CoreConf(..),
  defaultCoreConf
  ) where

--Bucephalusモジュール
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Pads

--SDL関連
import qualified Graphics.UI.SDL as SDL

--Haskell標準
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

-- ゲームの状態定義
class GamePad p => GameState s p | s -> p where
  gameMainCore :: (p, s) -> IO s
  gameQuitCore :: s -> IO ()

  gameQuitCore _ = return ()

---------------------------------------------------------------------------------------------------
-- コア
---------------------------------------------------------------------------------------------------

coreStart :: (GamePad p, GameState s p) => CoreConf -> p -> s -> IO ()
coreStart conf defaultPad defaultState = do
  --初期化処理
  initSDL conf
  --メインループ
  (_, final) <- SDL.getTicks >>= mainLoop (defaultPad, defaultState)
  --終了
  gameQuitCore final
  quitSDL

mainLoop :: (GamePad p, GameState s p) => (p, s) -> Word32 -> IO (p, s)
mainLoop (pad, st) ago = do
  SDL.delay 1 --CPU負担軽減
  --フレーム間隔を一定に保つ
  t <- SDL.getTicks
  if ago + 16 > t then mainLoop (pad, st) ago
   else do
    --ゲーム処理実行
    ev <- SDL.pollEvent
    nPad <- return $ interpretPadEvent pad ev
    nState <- gameMainCore (nPad, st)
    --終了条件判定、再帰
    if isQuit ev then return (nPad, nState) else mainLoop (nPad, nState) t

  where
    --終了条件判定
    isQuit :: SDL.Event -> Bool
    isQuit (SDL.KeyUp key) = SDL.symKey key == SDL.SDLK_ESCAPE --ESCキー押下
    isQuit ev              = ev             == SDL.Quit        --画面が閉じられる
    isQuit _               = False

---------------------------------------------------------------------------------------------------
--SDL初期化
initSDL :: CoreConf -> IO ()
initSDL conf = let
  surfaceFlags = if fullScreen conf then [SDL.Fullscreen] else []
  in do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 640 480 32 surfaceFlags
    return ()

--終了
quitSDL :: IO ()
quitSDL = SDL.quit

