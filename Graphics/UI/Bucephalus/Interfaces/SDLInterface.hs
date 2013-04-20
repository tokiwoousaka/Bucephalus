{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.Bucephalus.Interfaces.SDLInterface (
  BuceInterface(..),
  defaultCoreConf,
  BucePicture(..),
  BuceSound(..),
  BuceMusic(..)
  ) where
import Graphics.UI.Bucephalus.Core.CoreConf
import Graphics.UI.Bucephalus.Type.Events
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Image      as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr
import qualified Graphics.UI.SDL.Mixer      as SDLm

-----------------------------------------------------------------------------------------------------
-- Definition of type
-----------------------------------------------------------------------------------------------------

-- | This type holding SDL surface.
newtype BucePicture = BucePicture { getBucePicture :: SDL.Surface } deriving (Show, Eq)

-- | This type holding SDL Music.
newtype BuceMusic = BuceMusic { getBuceMusic :: SDLm.Music } deriving (Show, Eq)

-- | This type holding SDL Chunk.
newtype BuceSound = BuceSound { getBuceSound :: SDLm.Chunk } deriving (Show, Eq)

-- | @defaultCoreConf@ is default core configuration to utilize SDL interface.
defaultCoreConf :: CoreConf BuceInterface 
defaultCoreConf = unitCoreConf { coreInterface = BuceInterface }

-----------------------------------------------------------------------------------------------------

-- | This Type takes a role to call Bucephalus library from SDL library.
data BuceInterface = BuceInterface

instance CoreInterface BuceInterface BucePicture BuceMusic BuceSound where
  bucephalusInit conf = let
    --固定値
    stereo = 2
    mixDefaultFrequency = 22050
    surfaceFlags = if fullScreen conf then [SDL.Fullscreen] else []
    in do
      SDL.init [SDL.InitEverything]
      SDL.setVideoMode 640 480 32 surfaceFlags
      SDLm.openAudio mixDefaultFrequency SDLm.AudioS16Sys stereo 1024
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

  bucephalusFreePicture _ = SDL.freeSurface . getBucePicture

  bucephalusRotoZoom _ x roto size smoot = 
    fmap BucePicture $ SDLr.rotozoom (getBucePicture x) roto size smoot 

  bucephalusGetWidth _ = SDL.surfaceGetWidth . getBucePicture

  bucephalusGetHeight _ = SDL.surfaceGetHeight . getBucePicture

  bucephalusGetScreen _ = fmap BucePicture SDL.getVideoSurface

  bucephalusFlip _ = SDL.flip . getBucePicture

  bucephalusFillRect _ b rect color 
    = SDL.fillRect (getBucePicture b) (fmap tupleToRect rect) (SDL.Pixel color) >> return ()

  bucephalusLoadWav _ fname = fmap BuceSound $ SDLm.loadWAV fname

  bucephalusLoadMusic _ fname = fmap BuceMusic $ SDLm.loadMUS fname

  bucephalusPlayMusic _ d r = SDLm.playMusic (getBuceMusic d) r

  bucephalusPlaySound _ i d c = SDLm.playChannel i (getBuceSound d) c >> return ()

-----------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------

tupleToRect (x, y, w, h) = SDL.Rect x y w h

convertEvent :: SDL.Event -> BucephalusEvent
convertEvent (SDL.KeyDown (SDL.Keysym key _ _)) = BucephalusKeyDown $ convertKeyType key
convertEvent (SDL.KeyUp   (SDL.Keysym key _ _)) = BucephalusKeyUp   $ convertKeyType key
convertEvent SDL.Quit                           = BucephalusQuit
convertEvent _                                  = BucephalusNoEvent

convertKeyType :: SDL.SDLKey -> BucephalusKey
convertKeyType SDL.SDLK_ESCAPE = BucephalusKey_Esc 
convertKeyType SDL.SDLK_UP     = BucephalusKey_Up
convertKeyType SDL.SDLK_DOWN   = BucephalusKey_Down
convertKeyType SDL.SDLK_LEFT   = BucephalusKey_Left
convertKeyType SDL.SDLK_RIGHT  = BucephalusKey_Right
convertKeyType SDL.SDLK_a      = BucephalusKey_a
convertKeyType SDL.SDLK_b      = BucephalusKey_b
convertKeyType SDL.SDLK_c      = BucephalusKey_c
convertKeyType SDL.SDLK_d      = BucephalusKey_d
convertKeyType SDL.SDLK_e      = BucephalusKey_e
convertKeyType SDL.SDLK_f      = BucephalusKey_f
convertKeyType SDL.SDLK_g      = BucephalusKey_g
convertKeyType SDL.SDLK_h      = BucephalusKey_h
convertKeyType SDL.SDLK_i      = BucephalusKey_i
convertKeyType SDL.SDLK_j      = BucephalusKey_j
convertKeyType SDL.SDLK_k      = BucephalusKey_k
convertKeyType SDL.SDLK_l      = BucephalusKey_l
convertKeyType SDL.SDLK_m      = BucephalusKey_m
convertKeyType SDL.SDLK_n      = BucephalusKey_n
convertKeyType SDL.SDLK_o      = BucephalusKey_o
convertKeyType SDL.SDLK_p      = BucephalusKey_p
convertKeyType SDL.SDLK_q      = BucephalusKey_q
convertKeyType SDL.SDLK_r      = BucephalusKey_r
convertKeyType SDL.SDLK_s      = BucephalusKey_s
convertKeyType SDL.SDLK_t      = BucephalusKey_t
convertKeyType SDL.SDLK_u      = BucephalusKey_u
convertKeyType SDL.SDLK_v      = BucephalusKey_v
convertKeyType SDL.SDLK_w      = BucephalusKey_w
convertKeyType SDL.SDLK_x      = BucephalusKey_x
convertKeyType SDL.SDLK_y      = BucephalusKey_y
convertKeyType SDL.SDLK_z      = BucephalusKey_z
convertKeyType SDL.SDLK_RETURN = BucephalusKey_Return
convertKeyType _               = BucephalusKey_Unknown
