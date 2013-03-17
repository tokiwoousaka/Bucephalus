module Test.Type.Shape.Collision(allCollisionTests) where 

import Test.HUnit
import Graphics.UI.Bucephalus.Type.Shape.Collision

---------------------------------------------------------------------------------------------------

--四角形の当たり判定テスト
rectCollisionTests :: Test
rectCollisionTests = "Rectangle collision tests" ~: test [
  --テストケース１ ■leftの右下の点が■rightに入りこんでいる場合のテスト
  "Case01" ~: 
  (Rectangle (Point (10,10)) (Point (70,70))) `collision` 
  (Rectangle (Point (25,25)) (Point (100,100))) ~=? True,
  --テストケース２　■leftに■rightが内包されている場合のテスト
  "Case02" ~: 
  (Rectangle (Point (-100,-80)) (Point (70,10))) `collision` 
  (Rectangle (Point (0,0)) (Point (50,40))) ~=? True ,
  --テストケース３　■leftと■rightが横方向に隣り合いぶつかっていない場合のテスト
  "Case03" ~: 
  (Rectangle (Point (-110,0)) (Point (-60,50))) `collision` 
  (Rectangle (Point (-50,0)) (Point (0,50))) ~=? False ,
  --テストケース４ ■leftと■rightが縦方向に隣り合いぶつかっていない場合のテスト
  "Case04" ~: 
  (Rectangle (Point (100,-110)) (Point (200,-60))) `collision` 
  (Rectangle (Point (100,-50)) (Point (200,0))) ~=? False ,
  --テストケース５ ■leftに■rightが内包されている場合のテスト
  "Case05" ~: 
  (Rectangle (Point (0,0)) (Point (100,100))) `collision` 
  (Rectangle (Point (10,20)) (Point (80,90))) ~=? True ,
  ----テストケース６ ■leftと■rightが四隅の点すべてにおいて一致(∴完全重複)している場合のテスト
  "Case06" ~: 
  (Rectangle (Point (0,0)) (Point (65535,65535))) `collision` 
  (Rectangle (Point (0,0)) (Point (65535,65535))) ~=? True ,
  --テストケース７ ■leftと■rightが■leftの右下の点■rightの左上の点で接している場合のテスト(★ぶつかってない扱い)
  "Case07" ~: 
  (Rectangle (Point (0,0)) (Point (255,255))) `collision` 
  (Rectangle (Point (255,255)) (Point (65535,65535))) ~=? False ,
  --テストケース8 ■leftと■rightが■leftの右辺■rightの左辺で接している場合のテスト(★ぶつかってない扱い)
  "Case08" ~: 
  (Rectangle (Point (0,0)) (Point (255,255))) `collision` 
  (Rectangle (Point (255,0)) (Point (255,512))) ~=? False ,
  ----テストケース9 ■leftと■rightが■leftの右辺■rightの左辺で接している場合のテスト(★ぶつかってる扱い)
  "Case09" ~: 
  (Rectangle (Point (0,0)) (Point (255,255))) `collision` 
  (Rectangle (Point (254,0)) (Point (255,512))) ~=? True ]

---------------------------------------------------------------------------------------------------

--全テスト
allCollisionTests :: Test
allCollisionTests = "Collision tests" ~: test [rectCollisionTests]

--main関数・・・テスト実行
main :: IO ()
main = runTestTT allCollisionTests >> return ()
