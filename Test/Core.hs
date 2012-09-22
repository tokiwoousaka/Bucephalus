module Test where
import Graphics.UI.Bucephalus.Core

main = coreStart $ SubstancesCore testinit testmain testquit

---------------------------------------------------------------------------------------------------

data TestState = TestState Int deriving Show

incSt :: TestState -> TestState
incSt (TestState x) = TestState (x + 1)

---------------------------------------------------------------------------------------------------

testinit :: IO TestState 
testinit = return $ TestState 0

testmain :: GameState TestState -> IO (GameState TestState)
testmain st = do
  s <- return $ takeState st
  print s
  return $ GameState (buttons st) (incSt s)

testquit :: TestState -> IO ()
testquit _ = putStrLn "Called quit function"
