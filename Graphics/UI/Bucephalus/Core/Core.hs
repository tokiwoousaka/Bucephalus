{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Graphics.UI.Bucephalus.Core.Core(
  GameState(..),
  coreStart,
  CoreConf(..),
  CoreInterface(..)
  ) where

-- Bucephalus modules
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Events
import Graphics.UI.Bucephalus.Type.Pads

-- from base
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- Definition of type class
---------------------------------------------------------------------------------------------------

-- | This is type class for holding game status. 
class (CoreInterface a b, GamePad p) => GameState a b s p | s -> p where

  -- | This function is main programm of your game.
  gameMainCore :: a -> (p, s) -> IO s
  -- | At the time of an end, bucephalus core program would call this function.
  gameQuitCore :: a -> s -> IO ()

  gameQuitCore _ _ = return ()

---------------------------------------------------------------------------------------------------
-- Main functions
---------------------------------------------------------------------------------------------------

-- | @coreStart@ function provide basic operations. 
--   e.g. : main loop operation ,end judging and so on.
coreStart :: (CoreInterface a b, GamePad p, GameState a b s p) => CoreConf a -> p -> s -> IO ()
coreStart conf defaultPad defaultState = do
  --初期化処理
  bucephalusInit conf
  --メインループ
  api <- return $ coreInterface conf
  (_, final) <- bucephalusGetTicks api >>= mainLoop api (defaultPad, defaultState)

  --終了
  gameQuitCore api final
  bucephalusQuit conf

mainLoop :: (CoreInterface a b, GamePad p, GameState a b s p) => a -> (p, s) -> Word32 -> IO (p, s)
mainLoop api (pad, st) ago = do
  bucephalusDelay api 1 --CPU負担軽減
  --フレーム間隔を一定に保つ
  t <- bucephalusGetTicks api
  if ago + 16 > t then mainLoop api (pad, st) ago
   else do
    --ゲーム処理実行
    ev <- bucephalusPollEvent api
    nPad <- return $ interpretPadEvent pad ev
    nState <- gameMainCore api (nPad, st)
    --終了条件判定、再帰
    if isQuit ev then return (nPad, nState) else mainLoop api (nPad, nState) t

  where
    --終了条件判定
    isQuit :: BucephalusEvent -> Bool
    isQuit (BucephalusKeyUp key) = key == BucephalusKey_Esc --ESCキー押下
    isQuit BucephalusQuit        = True                     --画面が閉じられる
    isQuit _                     = False

