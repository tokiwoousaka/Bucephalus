{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Bucephalus.SDLApi (
  BuceFrame(..),
  BuceAPI(..),
  defaultCoreConf
  ) where
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Events
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr

-----------------------------------------------------------------------------------------------------
-- 型定義
-----------------------------------------------------------------------------------------------------

newtype BuceFrame = BuceFrame { getBuceFrame :: SDL.Surface } deriving (Show, Eq)

type BucephalusCoreConf = CoreConf BuceAPI
defaultCoreConf :: BucephalusCoreConf
defaultCoreConf = unitCoreConf { coreInterface = BuceAPI }

-----------------------------------------------------------------------------------------------------

data BuceAPI = BuceAPI

instance CoreInterface BuceAPI BuceFrame where
  --bucephalusInit :: CoreConf a -> IO ()
  bucephalusInit conf = let
    surfaceFlags = if fullScreen conf then [SDL.Fullscreen] else []
    in do
      SDL.init [SDL.InitEverything]
      SDL.setVideoMode 640 480 32 surfaceFlags
      return ()

  --bucephalusQuit :: CoreConf a -> IO ()
  bucephalusQuit _ = SDL.quit

  --bucephalusGetTicks :: a -> IO Word32
  bucephalusGetTicks _ = SDL.getTicks

  --bucephalusPollEvent :: a -> bucephalusEvent
  bucephalusPollEvent _ = SDL.pollEvent >>= return . convertEvent

  --bucephalusDelay :: a -> Word32 -> IO ()
  bucephalusDelay _ = SDL.delay 

  --bucephalusLoadImg :: CoreFrame b => a -> String -> IO b
  bucephalusLoadImg _ fn = SDLi.load fn >>= return . BuceFrame

  --bucephalusBulitImg a
  --  -> b -> Maybe (Int, Int, Int, Int) 
  --  -> b -> Maybe (Int, Int, Int, Int) -> b -> IO ()
  bucephalusBulitImg _ from fromRect to toRect = do
      SDL.blitSurface 
        (getBuceFrame from) (fmap tupleToRect fromRect) 
        (getBuceFrame to)   (fmap tupleToRect toRect)
      return ()

  --bucephalusFreeImg :: a -> b -> IO ()
  bucephalusFreeImg _ x = SDL.freeSurface $ getBuceFrame x 

  --bucephalusRotoZoom :: a -> b -> Double -> Double -> Bool -> IO b
  bucephalusRotoZoom _ x roto size smoot = 
    fmap BuceFrame $ SDLr.rotozoom (getBuceFrame x) roto size smoot 

  --bucephalusGetWidth :: b -> Int 
  bucephalusGetWidth _ = SDL.surfaceGetWidth . getBuceFrame

  --bucephalusGetHeight :: b -> Int
  bucephalusGetHeight _ = SDL.surfaceGetHeight . getBuceFrame

  --bucephalusGetVideoFrame :: IO b
  bucephalusGetVideoFrame _ = fmap BuceFrame SDL.getVideoSurface

  --bucephalusFlip :: a -> b -> IO ()
  bucephalusFlip _ = SDL.flip . getBuceFrame

  --bucephalusFreeFrame :: a -> b -> IO ()
  bucephalusFreeFrame _ = SDL.freeSurface . getBuceFrame

  --bucephalusFillRect :: a -> b ->  Maybe (Int, Int, Int, Int) -> Word32 -> IO ()
  bucephalusFillRect _ b rect color 
    = SDL.fillRect (getBuceFrame b) (fmap tupleToRect rect) (SDL.Pixel color) >> return ()

-----------------------------------------------------------------------------------------------------

--convert tuple to rect
tupleToRect (x, y, w, h) = SDL.Rect x y w h

--convert sdl event to bucephalus event
convertEvent :: SDL.Event -> BucephalusEvent
convertEvent SDL.Quit = BucephalusQuit
convertEvent _        = BucephalusNoEvent
