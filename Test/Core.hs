module Test where
import Graphics.UI.Bucephalus.Core

---------------------------------------------------------------------------------------------------
--Bucephalus Core テストプログラム

main = animationMain

---------------------------------------------------------------------------------------------------
-- アニメーションテスト
---------------------------------------------------------------------------------------------------

animationMain :: IO ()
animationMain = coreStart $ SubstancesCore {
  initSC = return $ TestState 0,
  mainSC = testmain,
  quitSC = \_ -> putStrLn "Game finished!!"
  }

---------------------------------------------------------------------------------------------------
-- 型定義

data TestState = TestState Int deriving Show
incSt :: TestState -> TestState
incSt (TestState x) = TestState (x + 1)

---------------------------------------------------------------------------------------------------

testmain :: GameState ButtonsState TestState -> IO (GameState ButtonsState TestState)
testmain st = do
  s <- return $ takeState st
  print s
  return $ GameState (buttons st) (incSt s)
