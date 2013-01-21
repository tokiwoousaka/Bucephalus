{-# LANGUAGE MultiParamTypeClasses #-}

module Core where
import Graphics.UI.Bucephalus.Core
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Image      as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr
import qualified Graphics.UI.SDL.Mixer      as SDLm

---------------------------------------------------------------------------------------------------
--Bucephalus Core テストプログラム

main :: IO ()
main = testProgram 2 --引数切り替えて実行するテストを変える

testProgram :: Int -> IO ()
testProgram 0 = animationMain
testProgram 1 = padTestMain
testProgram 2 = soundTestMain

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data OnpuState = OnpuState {
  onpuSurface  :: SDL.Surface, 
  onpuVector :: (Int, Int), 
  onpuPosition :: (Int, Int)} deriving (Show, Eq)

--おんぷ初期化
initOnpuState :: (Int, Int) -> (Int, Int) -> String -> IO OnpuState
initOnpuState (mx, my) (x, y) fnm = do
  sfc <- SDLi.load fnm
  return $ OnpuState sfc (mx, my) (x, y)

--おんぷをVector方向に移動
moveOnpu :: OnpuState -> OnpuState
moveOnpu onpu = let
  (mx, my) = onpuVector onpu
  (px, py) = onpuPosition onpu
  in onpu { onpuPosition = (mx + px, my + py) }

--画面端まできたおんぷをバウンドさせる
boundOnpu :: OnpuState -> OnpuState
boundOnpu onpu@(OnpuState _ (vx, vy) (px, py))
  | px < 0    = onpu { onpuVector = (abs vx      , vy          ) }
  | py < 0    = onpu { onpuVector = (vx          , abs vy      ) }
  | px > 540  = onpu { onpuVector = (0 - (abs vx), vy          ) }
  | py > 380  = onpu { onpuVector = (vx          , 0 - (abs vy)) }
  | otherwise = onpu

---------------------------------------------------------------------------------------------------
--画像を転送する
blitOnpu :: Double -> SDL.Surface -> OnpuState -> IO ()
blitOnpu size screen onpu = do
  --データ取り出し
  img <- return $ onpuSurface onpu
  (px, py) <- return $ onpuPosition onpu
  --ズーム処理
  imgB <- SDLr.rotozoom img 0 size False
  (x, y, w, h) <- return $ calcZoomRect img imgB px py --座標補正
  --screenに転送
  SDL.blitSurface 
    imgB   (Just $ SDL.Rect 0 0 w h) 
    screen (Just $ SDL.Rect x y w h) 
  --ズーム処理用一時サーフェス解放
  SDL.freeSurface imgB

--おんぷの画像を振動させる計算
vibration :: (Num a, Ord a) => a -> a -> a
vibration cy x 
  | x > cy    = 1
  | otherwise = x + 1

--zoom前後のサーフェスから座標補正
calcZoomRect :: SDL.Surface -> SDL.Surface -> Int -> Int -> (Int, Int, Int, Int) 
calcZoomRect bef aft x y =
  let
    (bw, bh) = getSurfaceSize bef
    (aw, ah) = getSurfaceSize aft
    x' = (-) x $ (ah - bh) `div` 2
    y' = (-) y $ (aw - bw) `div` 2
    in (x', y', aw, ah)

--サーフェスから幅と高さを取得
getSurfaceSize :: SDL.Surface -> (Int, Int)
getSurfaceSize sur = (SDL.surfaceGetWidth sur, SDL.surfaceGetHeight sur)

---------------------------------------------------------------------------------------------------
-- アニメーションテスト
---------------------------------------------------------------------------------------------------

animationMain = initAnimation >>= coreStart defaultCoreConf (padInit :: StandardPad)

--初期化
initAnimation :: IO AnimationState
initAnimation = do
  onpu <- sequence $ [
    initOnpuState (1 , 1 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState (2 , 2 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState (3 , 3 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState (4 , 4 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState (5 , 5 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState (6 , 6 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState (7 , 7 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState (8 , 8 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState (9 , 9 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState (10, 10) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState (11, 11) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState (12, 12) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState (13, 13) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState (14, 14) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState (15, 15) (0, 0) "Test/resources/OnpuR.png"]
  return $ AnimationState (onpu, 0)

---------------------------------------------------------------------------------------------------
--型定義
data AnimationState = AnimationState ([OnpuState], Integer)
instance GameState AnimationState StandardPad where
  --主処理
  gameMainCore (_, AnimationState (onpu, cnt)) = do
    --データ取得
    screen <- SDL.getVideoSurface
    nextCnt <- return $ vibration 25 cnt
    --ズームサイズ
    size <- return . (+1) $ (fromIntegral cnt) / 100
    --描画
    SDL.fillRect screen Nothing (SDL.Pixel 0)
    mapM_ (blitOnpu size screen) onpu 
    SDL.flip screen
    --フレーム処理終了
    return $ AnimationState (map (boundOnpu . moveOnpu) onpu, nextCnt)
   
  --終了処理
  gameQuitCore (AnimationState (onpu, _)) = mapM_ SDL.freeSurface $ map onpuSurface onpu

-----------------------------------------------------------------------------------------------------
---- キー／ゲームパッド入力テスト
-----------------------------------------------------------------------------------------------------

padTestMain = initPadTest >>= coreStart defaultCoreConf padInit 

initPadTest :: IO PadTestState 
initPadTest = do
  onpu <- initOnpuState (0, 0) (0, 0) "Test/resources/OnpuG.png"
  return $ PadTestState (onpu, 0)

-----------------------------------------------------------------------------------------------------

data PadTestState = PadTestState (OnpuState, Integer)
instance GameState PadTestState StandardPad where
  gameMainCore (ps, PadTestState (onpu, cnt)) = do
    --データ取得
    screen <- SDL.getVideoSurface
    nextCnt <- return $ vibration 25 cnt
    --ズームサイズ
    size <- return . (+1) $ (fromIntegral cnt) / 100
    --描画
    SDL.fillRect screen Nothing (SDL.Pixel 0)
    blitOnpu size screen onpu 
    SDL.flip screen

    nOnpu <- return $ onpu { onpuVector = mulVector.padToVector $ ps }
    --フレーム処理終了
    return $ PadTestState (boundOnpu . moveOnpu $ nOnpu, nextCnt)

      where mulVector (x, y) = (x * 3, y * 3)

  --終了処理
  gameQuitCore (PadTestState (onpu, _)) = SDL.freeSurface $ onpuSurface onpu

-----------------------------------------------------------------------------------------------------
---- サウンド／BGMテスト
-----------------------------------------------------------------------------------------------------

soundTestMain :: IO ()
soundTestMain = coreStart defaultCoreConf padInit $ SoundTestState Nothing

-----------------------------------------------------------------------------------------------------

--データ定義
type OnpuSeState = (OnpuState, Integer, Int, StandardPad-> Integer, SDLm.Chunk)
data SoundTestState = 
  SoundTestState (Maybe (SDLm.Music, [OnpuSeState]))

getOnpuSeList :: SoundTestState -> [OnpuSeState]
getOnpuSeList (SoundTestState Nothing) = []
getOnpuSeList (SoundTestState (Just (_, res))) = res

setOnpuSeList :: SoundTestState -> [OnpuSeState] -> SoundTestState
setOnpuSeList (SoundTestState (Just (mus, _))) st = SoundTestState (Just (mus, st))

--主処理作成
instance GameState SoundTestState StandardPad where
  gameMainCore (ps, st) = do
    --初期化
    inited <- initSoundTestState st
    --描画＆効果音再生
    screen <- SDL.getVideoSurface
    SDL.fillRect screen Nothing (SDL.Pixel 0)
    nxOnpuList <- mapM (playSound screen ps) (getOnpuSeList inited)
    SDL.flip screen
    --フレーム処理終了
    return $ setOnpuSeList inited nxOnpuList

----------
--状態初期化＆BGM再生開始
initSoundTestState :: SoundTestState -> IO SoundTestState
initSoundTestState (SoundTestState Nothing) = let
  --固定値
  stereo = 2
  mixDefaultFrequency = 22050
  --補助
  initSeOnpu (x, y) getBtnf channel fName sName = do
    chunk <- SDLm.loadWAV sName
    onpu <- initOnpuState (0, 0) (x, y) fName 
    return (onpu, 0, channel, getBtnf, chunk)
  in do
    SDLm.openAudio mixDefaultFrequency SDLm.AudioS16Sys stereo 1024
    --サウンド／画像読み込み
    seOnpuList <- sequence $ [
      initSeOnpu (20 , 140) buttonA     1 "Test/resources/OnpuR.png" "Test/resources/se2.wav",
      initSeOnpu (145, 240) buttonB     1 "Test/resources/OnpuB.png" "Test/resources/se1.wav",
      initSeOnpu (320, 190) buttonLeft  2 "Test/resources/OnpuR.png" "Test/resources/se5.wav",
      initSeOnpu (420, 90 ) buttonUp    3 "Test/resources/OnpuG.png" "Test/resources/se6.wav",
      initSeOnpu (420, 290) buttonDown  4 "Test/resources/OnpuG.png" "Test/resources/se3.wav",
      initSeOnpu (520, 190) buttonRight 5 "Test/resources/OnpuB.png" "Test/resources/se4.wav"]
    --BGM再生
    mus <- SDLm.loadMUS "Test/resources/BLIZZARD.ogg"
    SDLm.playMusic mus (-1)
    --返却
    return $ SoundTestState (Just (mus, seOnpuList))
initSoundTestState st = return st

--描画×効果音再生
playSound :: SDL.Surface -> StandardPad -> OnpuSeState -> IO OnpuSeState
playSound screen ps (onpu, zoom, channel, getBtnf, chunk) = do
  nzoom <- if getBtnf ps == 1 
    then SDLm.playChannel channel chunk 0 >> return 100 else return zoom
  size <- return . (+1) $ (fromIntegral nzoom) / 100
  blitOnpu size screen onpu 
  return (onpu, dec2Zero nzoom, channel, getBtnf, chunk)
    where dec2Zero x = if x == 0 then 0 else x - 1
