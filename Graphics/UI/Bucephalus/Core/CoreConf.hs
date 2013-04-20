{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Bucephalus.Core.CoreConf where
import Graphics.UI.Bucephalus.Type.Events
import Data.Word (Word32)

---------------------------------------------------------------------------------------------------
-- Definition of data type
---------------------------------------------------------------------------------------------------

-- | This is the configuration data for bucephalus @Core@ program.
data CoreConf a = CoreConf {
  fullScreen :: Bool,
  coreInterface :: a
  }

---------------------------------------------------------------------------------------------------

-- | The default configration holding @unit@.
unitCoreConf :: CoreConf ()
unitCoreConf = CoreConf {
  fullScreen = False,
  coreInterface = ()
  }

---------------------------------------------------------------------------------------------------

-- | This type class provides interface for multi media libraly.
class CoreInterface a b m c | a -> b, a -> m, a -> c where
  bucephalusInit :: CoreConf a -> IO ()
  bucephalusQuit :: CoreConf a -> IO ()
  bucephalusGetTicks :: a -> IO Word32
  bucephalusPollEvent :: a -> IO BucephalusEvent
  bucephalusDelay :: a -> Word32 -> IO ()
  bucephalusLoadImg :: a -> String -> IO b
  bucephalusBulitImg :: a
    -> b -> Maybe (Int, Int, Int, Int) 
    -> b -> Maybe (Int, Int, Int, Int) -> IO ()
  bucephalusFreePicture :: a -> b -> IO ()
  bucephalusRotoZoom :: a -> b -> Double -> Double -> Bool -> IO b
  bucephalusGetWidth :: a -> b -> Int
  bucephalusGetHeight :: a -> b -> Int
  bucephalusGetScreen :: a -> IO b
  bucephalusFlip :: a -> b -> IO ()
  bucephalusFillRect :: a -> b ->  Maybe (Int, Int, Int, Int) -> Word32 -> IO ()
  bucephalusLoadMusic :: a -> String -> IO m
  bucephalusLoadWav :: a -> String -> IO c
  bucephalusPlayMusic :: a -> m -> Int -> IO ()
  bucephalusPlaySound :: a -> Int -> c -> Int -> IO ()
