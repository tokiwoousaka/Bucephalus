module Main where 
import Graphics.UI.Bucephalus.Type.Collision

---------------------------------------------------------------------------------------------------

 --テストケース１ ■leftの右下の点が■rightに入りこんでいる場合のテスト
test01 :: Bool
test01 = let
  left = Rectangle (10,10) (70,70)
  right = Rectangle (25,25) (100,100)
  in (left `collision` right) == True
  
 --テストケース２　■leftに■rightが内包されている場合のテスト
test02 :: Bool
test02 = let
  left = Rectangle (-100,-80) (70,10)
  right = Rectangle (0,0) (50,40)
  in (left `collision` right) == True
  
 --テストケース３　■leftと■rightが横方向に隣り合いぶつかっていない場合のテスト
test03 :: Bool
test03 = let
  left = Rectangle (-110,0) (-60,50)
  right = Rectangle (-50,0) (0,50)
  in (left `collision` right) == False

 --テストケース４ ■leftと■rightが縦方向に隣り合いぶつかっていない場合のテスト
test04 :: Bool
test04 = let
  left = Rectangle (100,-110) (200,-60)
  right = Rectangle (100,-50) (200,0)
  in (left `collision` right) == False
  
 --テストケース５ ■leftに■rightが内包されている場合のテスト
test05 :: Bool
test05 = let
  left = Rectangle (0,0) (100,100)
  right = Rectangle (10,20) (80,90)
  in (left `collision` right) == True
  
 --テストケース６ ■leftと■rightが四隅の点すべてにおいて一致(∴完全重複)している場合のテスト
test06 :: Bool
test06 = let
  left = Rectangle (0,0) (65535,65535)
  right = Rectangle (0,0) (65535,65535)
  in (left `collision` right) == True

 --テストケース７ ■leftと■rightが■leftの右下の点■rightの左上の点で接している場合のテスト(★ぶつかってない扱い)
test07 :: Bool
test07 = let
  left = Rectangle (0,0) (255,255)
  right = Rectangle (255,255) (65535,65535)
  in (left `collision` right) == False

 --テストケース8 ■leftと■rightが■leftの右辺■rightの左辺で接している場合のテスト(★ぶつかってない扱い)
test08 :: Bool
test08 = let
  left = Rectangle (0,0) (255,255)
  right = Rectangle (255,0) (255,512)
  in (left `collision` right) == False

 --テストケース9 ■leftと■rightが■leftの右辺■rightの左辺で接している場合のテスト(★ぶつかってる扱い)
test09 :: Bool
test09 = let
  left = Rectangle (0,0) (255,255)
  right = Rectangle (254,0) (255,512)
  in (left `collision` right) == True

rectTest :: (Bool, [(Int, Bool)])
rectTest = (\l -> (all id l ,zip [1..] l)) [test01, test02, test03, test04, test05, test06, test07, test08, test09]