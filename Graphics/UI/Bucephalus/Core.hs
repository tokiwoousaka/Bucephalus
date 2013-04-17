
---------------------------------------------------------------------------------------------------
-- 
-- Core Program in The Project Bucephalus (C)2012 Tokiwo Ousaka
-- 
---------------------------------------------------------------------------------------------------

module Graphics.UI.Bucephalus.Core(
  --型の提供
  GameState(..),
  --主処理
  coreStart,
  --from Bucephalus.Type.Pads
  StandardPad(..),
  padInit,
  padToVector,
  --Config
  CoreConf(..),
  CoreAPI(..)
  ) where

import Graphics.UI.Bucephalus.Core.Core
import Graphics.UI.Bucephalus.Type.Pads
