module Graphics.UI.Bucephalus.Core.CoreConf where
import Graphics.UI.Bucephalus.Type.Events
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data CoreConf a = CoreConf {
  fullScreen :: Bool,
  coreAPI :: a
  }

---------------------------------------------------------------------------------------------------
--デフォルトの設定

defaultCoreConf :: CoreAPI a => CoreConf a
defaultCoreConf = CoreConf { fullScreen = False, coreAPI = undefined }

class CoreAPI a where
  bucephalusInit :: CoreConf a -> IO ()
  bucephalusQuit :: CoreConf a -> IO ()
  bucephalusGetTicks :: a -> IO Word32 --SDL.getTicks
  bucephalusPollEvent :: a -> bucephalusEvent --SDL.pollEvent
  bucephalusDelay :: a -> Word32 -> IO ()

