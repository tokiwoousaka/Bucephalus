{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

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

unitCoreConf :: CoreConf ()
unitCoreConf = CoreConf {
  fullScreen = False,
  coreAPI = ()
  }

---------------------------------------------------------------------------------------------------
-- マルチメディアライブラリを呼び出すためのAPI

class CoreAPI a b | a -> b where
  bucephalusInit :: CoreConf a -> IO ()
  bucephalusQuit :: CoreConf a -> IO ()
  bucephalusGetTicks :: a -> IO Word32
  bucephalusPollEvent :: a -> IO BucephalusEvent
  bucephalusDelay :: a -> Word32 -> IO ()
  bucephalusLoadImg :: a -> String -> IO b
  bucephalusBulitImg :: a
    -> b -> Maybe (Int, Int, Int, Int) 
    -> b -> Maybe (Int, Int, Int, Int) -> IO ()
  bucephalusFreeImg :: a -> b -> IO ()
  bucephalusRotoZoom :: a -> b -> Double -> Double -> Bool -> IO b
  bucephalusGetWidth :: a -> b -> Int
  bucephalusGetHeight :: a -> b -> Int
  bucephalusGetVideoFrame :: a -> IO b
  bucephalusFlip :: a -> b -> IO ()
  bucephalusFreeFrame :: a -> b -> IO ()
  bucephalusFillRect :: a -> b ->  Maybe (Int, Int, Int, Int) -> Word32 -> IO ()
