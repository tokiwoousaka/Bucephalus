{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Bucephalus.SDLApi (
  BucePicture(..),
  BuceInterface(..),
  defaultCoreConf
  ) where
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Events
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr

-----------------------------------------------------------------------------------------------------
-- Definition of type
-----------------------------------------------------------------------------------------------------

-- | This type holding SDL surface.
newtype BucePicture = BucePicture { getBucePicture :: SDL.Surface } deriving (Show, Eq)

-- | @defaultCoreConf@ is default core configuration with SDL interface.
defaultCoreConf :: BucephalusCoreConf
defaultCoreConf = unitCoreConf { coreInterface = BuceInterface }

-----------------------------------------------------------------------------------------------------

-- @BucephalusInterface@ data type provide interface from SDL multi media library.
data BuceInterface = BuceInterface

instance CoreInterface BuceInterface BucePicture where
  bucephalusInit conf = let
    surfaceFlags = if fullScreen conf then [SDL.Fullscreen] else []
    in do
      SDL.init [SDL.InitEverything]
      SDL.setVideoMode 640 480 32 surfaceFlags
      return ()

  bucephalusQuit _ = SDL.quit

  bucephalusGetTicks _ = SDL.getTicks

  bucephalusPollEvent _ = SDL.pollEvent >>= return . convertEvent

  bucephalusDelay _ = SDL.delay 

  bucephalusLoadImg _ fn = SDLi.load fn >>= return . BucePicture

  bucephalusBulitImg _ from fromRect to toRect = do
      SDL.blitSurface 
        (getBucePicture from) (fmap tupleToRect fromRect) 
        (getBucePicture to)   (fmap tupleToRect toRect)
      return ()

  bucephalusFreeImg _ x = SDL.freeSurface $ getBucePicture x 

  bucephalusRotoZoom _ x roto size smoot = 
    fmap BucePicture $ SDLr.rotozoom (getBucePicture x) roto size smoot 

  bucephalusGetWidth _ = SDL.surfaceGetWidth . getBucePicture

  bucephalusGetHeight _ = SDL.surfaceGetHeight . getBucePicture

  bucephalusGetVideoFrame _ = fmap BucePicture SDL.getVideoSurface

  bucephalusFlip _ = SDL.flip . getBucePicture

  bucephalusFreeFrame _ = SDL.freeSurface . getBucePicture

  bucephalusFillRect _ b rect color 
    = SDL.fillRect (getBucePicture b) (fmap tupleToRect rect) (SDL.Pixel color) >> return ()

-----------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------

-- This function provide convert tuple to rect
tupleToRect (x, y, w, h) = SDL.Rect x y w h

-- This function provide convert sdl event to bucephalus event
convertEvent :: SDL.Event -> BucephalusEvent
convertEvent SDL.Quit = BucephalusQuit
convertEvent _        = BucephalusNoEvent
