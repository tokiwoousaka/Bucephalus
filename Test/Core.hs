{-# LANGUAGE MultiParamTypeClasses #-}

module Core where
import Graphics.UI.Bucephalus.Core
import Graphics.UI.Bucephalus.Interfaces.SDLInterface

---------------------------------------------------------------------------------------------------
--Bucephalus Core テストプログラム

main :: IO ()
main = do
  putStrLn "Select test program"
  putStrLn "-------------------"
  putStrLn "Animation Test - 0"
  putStrLn "Gamepad Test   - 1"
  putStrLn "Sound Test     - 2"
  putStrLn "Quit           - Other"
  putStrLn "-------------------"

  getLine >>= testProgram

testProgram :: String -> IO ()
testProgram "0" = animationMain
testProgram "1" = padTestMain
testProgram "2" = soundTestMain
testProgram _ = return ()

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data OnpuState = OnpuState {
  onpuSurface  :: BucePicture, 
  onpuVector :: (Int, Int), 
  onpuPosition :: (Int, Int)} deriving (Show, Eq)

--おんぷ初期化
initOnpuState :: BuceInterface -> (Int, Int) -> (Int, Int) -> String -> IO OnpuState
initOnpuState iFace (mx, my) (x, y) fnm = do
  sfc <- (bucephalusLoadImg iFace) fnm
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
blitOnpu :: BuceInterface -> Double -> BucePicture -> OnpuState -> IO ()
blitOnpu iFace size screen onpu = do
  --データ取り出し
  img <- return $ onpuSurface onpu
  (px, py) <- return $ onpuPosition onpu
  --ズーム処理
  imgB <- bucephalusRotoZoom iFace img 0 size False
  (x, y, w, h) <- return $ calcZoomRect iFace img imgB px py --座標補正
  --screenに転送
  bucephalusBulitImg iFace
    imgB   (Just (0, 0, w, h))
    screen (Just (x, y, w, h))
  --ズーム処理用一時サーフェス解放
  bucephalusFreePicture iFace imgB

--おんぷの画像を振動させる計算
vibration :: (Num a, Ord a) => a -> a -> a
vibration cy x 
  | x > cy    = 1
  | otherwise = x + 1

--zoom前後のサーフェスから座標補正
calcZoomRect :: BuceInterface -> BucePicture -> BucePicture -> Int -> Int -> (Int, Int, Int, Int) 
calcZoomRect iFace bef aft x y =
  let
    (bw, bh) = getSurfaceSize iFace bef
    (aw, ah) = getSurfaceSize iFace aft
    x' = (-) x $ (ah - bh) `div` 2
    y' = (-) y $ (aw - bw) `div` 2
    in (x', y', aw, ah)

--サーフェスから幅と高さを取得
getSurfaceSize :: BuceInterface -> BucePicture -> (Int, Int)
getSurfaceSize iFace sur = (bucephalusGetWidth iFace sur, bucephalusGetHeight iFace sur)

---------------------------------------------------------------------------------------------------
-- アニメーションテスト
---------------------------------------------------------------------------------------------------

--coreStart :: (CoreInterface a b, GamePad p, GameState s p) => CoreConf a -> p -> s -> IO ()
animationMain = initAnimation BuceInterface >>= coreStart defaultCoreConf (padInit :: StandardPad)

--初期化
initAnimation :: BuceInterface -> IO AnimationState
initAnimation iFace = do
  onpu <- sequence $ [
    initOnpuState iFace (1 , 1 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState iFace (2 , 2 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState iFace (3 , 3 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState iFace (4 , 4 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState iFace (5 , 5 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState iFace (6 , 6 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState iFace (7 , 7 ) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState iFace (8 , 8 ) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState iFace (9 , 9 ) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState iFace (10, 10) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState iFace (11, 11) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState iFace (12, 12) (0, 0) "Test/resources/OnpuR.png",
    initOnpuState iFace (13, 13) (0, 0) "Test/resources/OnpuG.png",
    initOnpuState iFace (14, 14) (0, 0) "Test/resources/OnpuB.png",
    initOnpuState iFace (15, 15) (0, 0) "Test/resources/OnpuR.png"]
  return $ AnimationState (onpu, 0)

---------------------------------------------------------------------------------------------------
--型定義
data AnimationState = AnimationState ([OnpuState], Integer)
instance GameState BuceInterface BucePicture BuceMusic BuceSound AnimationState StandardPad where
  --主処理
  --gameMainCore :: CoreInterface a b => a -> (p, s) -> IO s
  gameMainCore iFace (_, AnimationState (onpu, cnt)) = do
    --データ取得
    screen <- bucephalusGetScreen iFace
    nextCnt <- return $ vibration 25 cnt
    --ズームサイズ
    size <- return . (+1) $ (fromIntegral cnt) / 100
    --描画
    bucephalusFillRect iFace screen Nothing 0
    mapM_ (blitOnpu iFace size screen) onpu 
    bucephalusFlip iFace $ screen
    --フレーム処理終了
    return $ AnimationState (map (boundOnpu . moveOnpu) onpu, nextCnt)
   
  --終了処理
  gameQuitCore iFace (AnimationState (onpu, _)) = mapM_ (bucephalusFreePicture iFace) $ map onpuSurface onpu

-----------------------------------------------------------------------------------------------------
---- キー／ゲームパッド入力テスト
-----------------------------------------------------------------------------------------------------

padTestMain = initPadTest BuceInterface >>= coreStart defaultCoreConf padInit 

initPadTest :: BuceInterface -> IO PadTestState 
initPadTest iFace = do
  onpu <- initOnpuState iFace (0, 0) (0, 0) "Test/resources/OnpuG.png"
  return $ PadTestState (onpu, 0)

-----------------------------------------------------------------------------------------------------

data PadTestState = PadTestState (OnpuState, Integer)
instance GameState BuceInterface BucePicture BuceMusic BuceSound PadTestState StandardPad where
  gameMainCore iFace (ps, PadTestState (onpu, cnt)) = do
    --データ取得
    screen <- bucephalusGetScreen iFace
    nextCnt <- return $ vibration 25 cnt
    --ズームサイズ
    size <- return . (+1) $ (fromIntegral cnt) / 100
    --描画
    bucephalusFillRect iFace screen Nothing 0
    blitOnpu iFace size screen onpu 
    bucephalusFlip iFace screen

    nOnpu <- return $ onpu { onpuVector = mulVector.padToVector $ ps }
    --フレーム処理終了
    return $ PadTestState (boundOnpu . moveOnpu $ nOnpu, nextCnt)

      where mulVector (x, y) = (x * 3, y * 3)

  --終了処理
  gameQuitCore iFace (PadTestState (onpu, _)) = bucephalusFreePicture iFace $ onpuSurface onpu

----------------------------------------------------------------------------------------------------
---- サウンド／BGMテスト
-----------------------------------------------------------------------------------------------------

soundTestMain :: IO ()
soundTestMain = coreStart defaultCoreConf padInit $ SoundTestState Nothing

-----------------------------------------------------------------------------------------------------

--データ定義
type OnpuSeState = (OnpuState, Integer, Int, StandardPad-> Integer, BuceSound)
data SoundTestState = 
  SoundTestState (Maybe (BuceMusic, [OnpuSeState]))

getOnpuSeList :: SoundTestState -> [OnpuSeState]
getOnpuSeList (SoundTestState Nothing) = []
getOnpuSeList (SoundTestState (Just (_, res))) = res

setOnpuSeList :: SoundTestState -> [OnpuSeState] -> SoundTestState
setOnpuSeList (SoundTestState (Just (mus, _))) st = SoundTestState (Just (mus, st))

--主処理作成
instance GameState BuceInterface BucePicture BuceMusic BuceSound SoundTestState StandardPad where
  gameMainCore iFace (ps, st) = do
    --初期化
    inited <- initSoundTestState iFace st
    --描画＆効果音再生
    screen <- bucephalusGetScreen iFace
    bucephalusFillRect iFace screen Nothing 0
    nxOnpuList <- mapM (playSound iFace screen ps) (getOnpuSeList inited)
    bucephalusFlip iFace screen
    --フレーム処理終了
    return $ setOnpuSeList inited nxOnpuList

----------
--状態初期化＆BGM再生開始
initSoundTestState :: BuceInterface -> SoundTestState -> IO SoundTestState
initSoundTestState iFace (SoundTestState Nothing) = let
  --補助
  initSeOnpu (x, y) getBtnf channel fName sName = do
    chunk <- bucephalusLoadWav iFace sName
    onpu <- initOnpuState iFace (0, 0) (x, y) fName 
    return (onpu, 0, channel, getBtnf, chunk)
  in do
    --サウンド／画像読み込み
    seOnpuList <- sequence $ [
      initSeOnpu (20 , 140) buttonA     1 "Test/resources/OnpuR.png" "Test/resources/se2.wav",
      initSeOnpu (145, 240) buttonB     1 "Test/resources/OnpuB.png" "Test/resources/se1.wav",
      initSeOnpu (320, 190) buttonLeft  2 "Test/resources/OnpuR.png" "Test/resources/se5.wav",
      initSeOnpu (420, 90 ) buttonUp    3 "Test/resources/OnpuG.png" "Test/resources/se6.wav",
      initSeOnpu (420, 290) buttonDown  4 "Test/resources/OnpuG.png" "Test/resources/se3.wav",
      initSeOnpu (520, 190) buttonRight 5 "Test/resources/OnpuB.png" "Test/resources/se4.wav"]
    --BGM再生
    mus <- bucephalusLoadMusic iFace "Test/resources/BLIZZARD.ogg"
    bucephalusPlayMusic iFace mus (-1)
    --返却
    return $ SoundTestState (Just (mus, seOnpuList))
initSoundTestState _ st = return st

--描画×効果音再生
playSound :: BuceInterface -> BucePicture -> StandardPad -> OnpuSeState -> IO OnpuSeState
playSound iFace screen ps (onpu, zoom, channel, getBtnf, chunk) = do
  nzoom <- if getBtnf ps == 1 
    then bucephalusPlaySound iFace channel chunk 0 >> return 100 else return zoom
  size <- return . (+1) $ (fromIntegral nzoom) / 100
  blitOnpu iFace size screen onpu 
  return (onpu, dec2Zero nzoom, channel, getBtnf, chunk)
    where dec2Zero x = if x == 0 then 0 else x - 1
