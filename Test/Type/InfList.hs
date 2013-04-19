module InfList() where
import qualified Graphics.UI.Bucephalus.Type.InfList as INF

fromList = flip INF.fromList 0

test :: INF.List (Int, Int)
test = INF.zip (fromList [1,2,3,4]) (fromList [5,6,7])

main :: IO ()
main = print $ INF.take 100 test
