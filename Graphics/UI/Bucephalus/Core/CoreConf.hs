module Graphics.UI.Bucephalus.Core.CoreConf(
  CoreConf(..), 
  defaultCoreConf
  ) where

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data CoreConf = CoreConf {
  fullScreen :: Bool
  }

---------------------------------------------------------------------------------------------------
--デフォルトの設定

defaultCoreConf :: CoreConf
defaultCoreConf = CoreConf { fullScreen = False }


