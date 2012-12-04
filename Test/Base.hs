
module Base where
import Graphics.UI.Bucephalus.Base

---------------------------------------------------------------------------------------------------
--Bucephalus Base テストプログラム

main :: IO ()
main = print $ moveObject id (StandardGameObject (StandardObjectState, StandardObjectState))
