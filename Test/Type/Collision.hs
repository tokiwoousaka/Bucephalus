module Main where 
import Graphics.UI.Bucephalus.Type.Collision

---------------------------------------------------------------------------------------------------

--テストケース１
test01 :: Bool
test01 = let
  left = Rectangle (10,10) (70,70)
  right = Rectangle (25,25) (100,100)
  in (left `collision` right) == True
  
  --テストケース２ 
test02 :: Bool
test02 = let
  left = Rectangle (-100,-80) (70,10)
  right = Rectangle (0,0) (50,40)
  in (left `collision` right) == True
  
  --テストケース３
test03 :: Bool
test03 = let
  left = Rectangle (-110,0) (-60,50)
  right = Rectangle (-50,0) (0,50)
  in (left `collision` right) == False

 --テストケース４
test04 :: Bool
test04 = let
  left = Rectangle (100,-110) (200,-60)
  right = Rectangle (100,-50) (200,0)
  in (left `collision` right) == False
  
 --テストケース５
test05 :: Bool
test05 = let
  left = Rectangle (0,0) (100,100)
  right = Rectangle (10,20) (80,90)
  in (left `collision` right) == True
  
 --テストケース６
test06 :: Bool
test06 = let
  left = Rectangle (0,0) (65535,65535)
  right = Rectangle (0,0) (65535,65535)
  in (left `collision` right) == True

rectTest :: (Bool, [(Int, Bool)])
rectTest = (\l -> (all id l ,zip [1..] l)) [test01, test02, test03, test04, test05, test06]