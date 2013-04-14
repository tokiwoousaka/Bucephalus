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
import Graphics.UI.Bucephalus.Type.Events
import Graphics.UI.Bucephalus.Type.Pads

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

coreStart :: (CoreAPI a, GamePad p, GameState s p) => CoreConf a -> p -> s -> IO ()
coreStart conf defaultPad defaultState = do
  --初期化処理
  bucephalusInit conf
  --メインループ
  api <- return $ coreAPI conf
  (_, final) <- bucephalusGetTicks api >>= mainLoop api (defaultPad, defaultState)

  --終了
  gameQuitCore final
  bucephalusQuit conf

mainLoop :: (CoreAPI a, GamePad p, GameState s p) => a -> (p, s) -> Word32 -> IO (p, s)
mainLoop api (pad, st) ago = do
  bucephalusDelay api 1 --CPU負担軽減
  --フレーム間隔を一定に保つ
  t <- bucephalusGetTicks api
  if ago + 16 > t then mainLoop api (pad, st) ago
   else do
    --ゲーム処理実行
    ev <- bucephalusPollEvent api
    nPad <- return $ interpretPadEvent pad ev
    nState <- gameMainCore (nPad, st)
    --終了条件判定、再帰
    if isQuit ev then return (nPad, nState) else mainLoop api (nPad, nState) t

  where
    --終了条件判定
    isQuit :: BucephalusEvent -> Bool
    isQuit (BucephalusKeyUp key) = key == BucephalusKey_Esc --ESCキー押下
    isQuit BucephalusQuit        = True                     --画面が閉じられる
    isQuit _                     = False

