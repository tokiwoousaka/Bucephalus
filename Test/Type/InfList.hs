module InfList() where
import qualified Graphics.UI.Bucephalus.Type.InfList as INF

test :: INF.List (Int, Int)
test = INF.zip (INF.fromList [1,2,3]) (INF.fromList [5,6,7])

main :: IO ()
main = print $ INF.take 100 test
