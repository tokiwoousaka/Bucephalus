module Core where
import Graphics.UI.Bucephalus.Core
import qualified Graphics.UI.SDL            as SDL
import qualified Graphics.UI.SDL.Image      as SDLi
import qualified Graphics.UI.SDL.Rotozoomer as SDLr

---------------------------------------------------------------------------------------------------
--Bucephalus Core テストプログラム

main :: IO ()
main = testProgram 1

testProgram :: Int -> IO ()
testProgram 0 = animationMain
testProgram 1 = padTestMain

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

animationMain :: IO ()
animationMain = coreStart $ SubstancesCore {
  initSC = initAnimation,
  mainSC = mainAnimation,
  quitSC = quitAnimation
  }

---------------------------------------------------------------------------------------------------

--初期化
initAnimation :: IO ([OnpuState], Integer)
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
  return (onpu, 0)

--主処理
mainAnimation :: (GameState StanderdPad ([OnpuState], Integer)) 
                  -> IO (GameState StanderdPad ([OnpuState], Integer))
mainAnimation (GameState bs (onpu, cnt)) = do
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
  nextState <- return (map (boundOnpu . moveOnpu) onpu, nextCnt)
  return $ (GameState bs nextState)
 
--終了処理
quitAnimation :: ([OnpuState], Integer) -> IO ()
quitAnimation (onpu, _) = mapM_ SDL.freeSurface $ map onpuSurface onpu

---------------------------------------------------------------------------------------------------
-- キー／ゲームパッド入力テスト
---------------------------------------------------------------------------------------------------

padTestMain :: IO ()
padTestMain = coreStart $ SubstancesCore {
  initSC = initPadTest,
  mainSC = mainPadTest,
  quitSC = quitPadTest 
  }

---------------------------------------------------------------------------------------------------

initPadTest :: IO (OnpuState, Integer)
initPadTest = do
  onpu <- initOnpuState (0, 0) (0, 0) "Test/resources/OnpuG.png"
  return (onpu, 0)

mainPadTest :: (GameState StanderdPad (OnpuState, Integer)) 
                  -> IO (GameState StanderdPad (OnpuState, Integer))
mainPadTest (GameState bs (onpu, cnt)) = do
  --データ取得
  screen <- SDL.getVideoSurface
  nextCnt <- return $ vibration 25 cnt
  --ズームサイズ
  size <- return . (+1) $ (fromIntegral cnt) / 100
  --描画
  SDL.fillRect screen Nothing (SDL.Pixel 0)
  blitOnpu size screen onpu 
  SDL.flip screen
  --フレーム処理終了
  nextState <- return (boundOnpu . moveOnpu $ onpu, nextCnt)
  return $ (GameState bs nextState)

--終了処理
quitPadTest :: (OnpuState, Integer) -> IO ()
quitPadTest (onpu, _) = SDL.freeSurface $ onpuSurface onpu
